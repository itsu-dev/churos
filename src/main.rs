#![no_std]
#![no_main]
#![feature(offset_of)]
#![feature(iter_array_chunks)]

use core::arch::asm;
use core::cmp::min;
use core::mem::offset_of;
use core::mem::size_of;
use core::panic::PanicInfo;
use core::ptr::null_mut;

type EfiVoid = u8;
type EfiHandle = u64;
type Result<T> = core::result::Result<T, &'static str>;

const FONT_WIDTH: usize = 8;
const FONT_HEIGHT: usize = 16;
type Font = [[bool; FONT_WIDTH]; FONT_HEIGHT];
type Fonts = [Font; 256];

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[must_use]
#[repr(u64)]
enum EfiStatus {
    Success = 0,
}

#[repr(C)]
struct EfiBootServicesTable {
    _reserved0: [u64; 40],
    locate_protocol: extern "win64" fn(
        protocol: *const EfiGuid,
        registration: *const EfiVoid,
        interface: *mut *mut EfiVoid,
    ) -> EfiStatus,
}
const _: () = assert!(offset_of!(EfiBootServicesTable, locate_protocol) == 320);

#[repr(C)]
struct EfiSystemTable {
    _reserved0: [u64; 12],
    pub boot_services: &'static EfiBootServicesTable,
}
const _: () = assert!(offset_of!(EfiSystemTable, boot_services) == 96);

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct EfiGuid {
    pub data0: u32,
    pub data1: u16,
    pub data2: u16,
    pub data3: [u8; 8],
}

#[repr(C)]
#[derive(Debug)]
struct EfiGraphicsOutputProtocol<'a> {
    reserved: [u64; 3],
    pub mode: &'a EfiGraphicsOutputProtocolMode<'a>,
}

#[repr(C)]
#[derive(Debug)]
struct EfiGraphicsOutputProtocolMode<'a> {
    pub max_mode: u32,
    pub mode: u32,
    pub info: &'a EfiGraphicsOutputProtocolPixelInfo,
    pub size_of_info: u64,
    pub frame_buffer_base: usize,
    pub frame_buffer_size: usize,
}

#[repr(C)]
#[derive(Debug)]
struct EfiGraphicsOutputProtocolPixelInfo {
    version: u32,
    /// 水平方向の画素数
    pub horizontal_resolution: u32,
    /// 垂直方向の画素数
    pub vertical_resolution: u32,
    _padding0: [u32; 5],
    /// 水平方向のデータに含まれる画素数
    pub pixels_per_scan_live: u32,
}
const _: () = assert!(size_of::<EfiGraphicsOutputProtocolPixelInfo>() == 36);

const EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID: EfiGuid = EfiGuid {
    data0: 0x9042a9de,
    data1: 0x23dc,
    data2: 0x4a38,
    data3: [0x96, 0xfb, 0x7a, 0xde, 0xd0, 0x80, 0x51, 0x6a],
};

fn locate_graphic_protocol<'a>(
    efi_system_table: &EfiSystemTable,
) -> Result<&'a EfiGraphicsOutputProtocol<'a>> {
    let mut graphic_output_protocol = null_mut::<EfiGraphicsOutputProtocol>();
    let status = (efi_system_table.boot_services.locate_protocol)(
        &EFI_GRAPHICS_OUTPUT_PROTOCOL_GUID,
        null_mut::<EfiVoid>(),
        &mut graphic_output_protocol as *mut *mut EfiGraphicsOutputProtocol as *mut *mut EfiVoid,
    );
    if status != EfiStatus::Success {
        return Err("Failed to locate graphics output protocol!");
    }
    Ok(unsafe { &*graphic_output_protocol })
}

trait Bitmap {
    fn bytes_per_pixel(&self) -> i64;
    fn pixels_per_line(&self) -> i64;
    fn width(&self) -> i64;
    fn height(&self) -> i64;
    fn buf_mut(&mut self) -> *mut u8;

    /// # Safety
    /// 
    /// (x, y) must be a valid point in the buf.
    unsafe fn unchecked_pixel_at_mut(&mut self, x: i64, y: i64) -> *mut u32 {
        self.buf_mut()
            .add(((y * self.pixels_per_line() + x) * self.bytes_per_pixel()) as usize)
            as *mut u32
    }

    fn pixel_at_mut(&mut self, x: i64, y: i64) -> Option<&mut u32> {
        if self.is_in_x_range(x) && self.is_in_y_range(y) {
            unsafe { Some(&mut *(self.unchecked_pixel_at_mut(x, y))) }
        } else {
            None
        }
    }

    fn is_in_x_range(&self, px: i64) -> bool {
        0 <= px && px < min(self.width(), self.pixels_per_line())
    }

    fn is_in_y_range(&self, py: i64) -> bool {
        0 <= py && py < self.height()
    }
}

#[derive(Clone, Debug)]
struct VramBufferInfo {
    buf: *mut u8,
    width: i64,
    height: i64,
    pixels_per_line: i64,
}

impl Bitmap for VramBufferInfo {
    fn bytes_per_pixel(&self) -> i64 {
        4
    }

    fn pixels_per_line(&self) -> i64 {
        self.pixels_per_line
    }

    fn width(&self) -> i64 {
        self.width
    }

    fn height(&self) -> i64 {
        self.height
    }

    fn buf_mut(&mut self) -> *mut u8 {
        self.buf
    }
}

fn init_vram(efi_system_table: &EfiSystemTable) -> Result<VramBufferInfo> {
    let gp = locate_graphic_protocol(efi_system_table)?;
    Ok(VramBufferInfo {
        buf: gp.mode.frame_buffer_base as *mut u8,
        width: gp.mode.info.horizontal_resolution as i64,
        height: gp.mode.info.vertical_resolution as i64,
        pixels_per_line: gp.mode.info.pixels_per_scan_live as i64,
    })
}

fn init_bdf_font() -> Option<Fonts> {
    // BDF のテキストデータ
    const FONT_SOURCE: &str = include_str!("../third_party/font/shnm8x16r.bdf");

    // 結果セット
    let mut fonts = [[[false; FONT_WIDTH]; FONT_HEIGHT]; 256];

    // BDF の各行
    let mut lines = FONT_SOURCE.split('\n');
    // 現在 BITMAP セクションにいるかどうか
    let mut is_bitmap = false;
    // 現在パース中の文字コード
    let mut char_code: usize = 0;
    // ビットマップの行番号
    let mut row = 0;

    while let Some(line) = lines.next() {
        if line.starts_with("ENDCHAR") {
            is_bitmap = false;
            row = 0;
            continue;
        }
        
        if is_bitmap {
            let mut column = 0;
            for c in line.chars() {
                // char -> &str
                let bytes = [c as u8];
                let s: &str = unsafe { core::str::from_utf8_unchecked(&bytes) };

                let hex = u8::from_str_radix(s, 16).unwrap();
                fonts[char_code][row][column * 4 + 0] = hex & 0b1000 != 0;
                fonts[char_code][row][column * 4 + 1] = hex & 0b0100 != 0;
                fonts[char_code][row][column * 4 + 2] = hex & 0b0010 != 0;
                fonts[char_code][row][column * 4 + 3] = hex & 0b0001 != 0;

                column += 1;
            }

            row += 1;
            continue;
        }

        if line.starts_with("STARTCHAR") {
            char_code = usize::from_str_radix(line.split(' ').last().unwrap(), 16).unwrap();
            continue;
        }

        if line.starts_with("BITMAP") {
            is_bitmap = true;
            continue;
        }
    }

    Some(fonts)
}

fn lookup_font(fonts: &Fonts, c: char) -> Option<Font> {
    Some(fonts[c as usize])
}

fn draw_char<T: Bitmap>(buf: &mut T, color: u32, fonts: &Fonts, c: char, x: i64, y: i64) -> Result<()> {
    let font = lookup_font(fonts, c).unwrap();

    for fy in 0..FONT_HEIGHT {
        for fx in 0..FONT_WIDTH {
            if font[fy][fx] {
                let _ = draw_point(buf, color, x + fx as i64, y + fy as i64);
            }
        }
    }

    Ok(())
}

fn draw_str<T: Bitmap>(buf: &mut T, color: u32, fonts: &Fonts, str: &str, x: i64, y: i64) -> Result<()> {
    for (i, c) in str.chars().enumerate() {
        let _ = draw_char(buf, color, fonts, c, x + (i * FONT_WIDTH) as i64, y);
    }
    Ok(())
}

/// # Safety
/// 
/// (x, y) must be a valid point in the buf.
unsafe fn unchecked_draw_point<T: Bitmap>(
    buf: &mut T,
    color: u32,
    x: i64,
    y: i64,
) {
    *buf.unchecked_pixel_at_mut(x, y) = color;
}

fn draw_point<T: Bitmap>(
    buf: &mut T,
    color: u32,
    x: i64,
    y: i64,
) -> Result<()> {
    *(buf.pixel_at_mut(x, y).ok_or("Out of Range!")?) = color;
    Ok(())
}

fn fill_rect<T: Bitmap>(
    buf: &mut T,
    color: u32,
    px: i64,
    py: i64,
    w: i64,
    h: i64,
) -> Result<()> {
    if !buf.is_in_x_range(px)
        || !buf.is_in_y_range(py)
        || !buf.is_in_x_range(px + w - 1)
        || !buf.is_in_y_range(py + h - 1)
    {
        return Err("Out of Range!");
    }

    for y in py..py + h {
        for x in px..px + w {
            unsafe {
                unchecked_draw_point(buf, color, x, y);
            }
        }
    }

    Ok(())
}

fn calc_slope_point(da: i64, db: i64, ia: i64) -> Option<i64> {
    if da < db {
        None
    } else if da == 0 {
        Some(0)
    } else if (0..=da).contains(&ia) {
        Some((2 * db * ia + da) / da / 2)
    } else {
        None
    }
}

fn draw_line<T: Bitmap>(
    buf: &mut T,
    color: u32,
    x0: i64,
    y0: i64,
    x1: i64,
    y1: i64,
) -> Result<()> {
    if !buf.is_in_x_range(x0)
    || !buf.is_in_x_range(x1)
    || !buf.is_in_y_range(y0)
    || !buf.is_in_y_range(y1)
    {
        return Err("Out of Range!");
    }   

    let dx = (x1 - x0).abs();
    let sx = (x1 - x0).signum();
    let dy = (y1 - y0).abs();
    let sy = (y1 - y0).signum();

    if dx >= dy {
        for (rx, ry) in (0..dx)
            .flat_map(|rx| calc_slope_point(dx, dy, rx).map(|ry| (rx, ry)))
        {
            draw_point(buf, color, x0 + rx *& sx, y0 + ry * sy)?;
        }
    } else {
        for (rx, ry) in (0..dy)
            .flat_map(|ry| calc_slope_point(dy, dx, ry).map(|rx: i64| (rx, ry)))
        {
            draw_point(buf, color, x0 + rx *& sx, y0 + ry * sy)?;
        }
    }

    Ok(())
}

#[no_mangle]
fn efi_main(_image_handle: EfiHandle, efi_system_table: &EfiSystemTable) {
    let mut vram = init_vram(efi_system_table).expect("Failed to init vram!");
    let fonts = init_bdf_font().unwrap();
    
    let vw = vram.width;
    let vh = vram.height;
    fill_rect(&mut vram, 0x000000, 0, 0, vw, vh).expect("fill_rect failed!");
    fill_rect(&mut vram, 0xff0000, 32, 32, 32, 32).expect("fill_rect failed!");
    fill_rect(&mut vram, 0x00ff00, 64, 64, 64, 64).expect("fill_rect failed!");
    fill_rect(&mut vram, 0x0000ff, 128, 128, 128, 128).expect("fill_rect failed!");
    
    for i in 0..256 {
        let _ = draw_point(&mut vram, 0x010101 * i as u32, i, i);
    }

    let grid_size: i64 = 32;
    let rect_size: i64 = grid_size * 8;
    for i in (0..=rect_size).step_by(grid_size as usize) {
        let _ =  draw_line(&mut vram, 0xff0000, 0, i, rect_size, i);
        let _ =  draw_line(&mut vram, 0xff0000, i, 0, i, rect_size);
    }

    let cx = rect_size / 2;
    let cy = rect_size / 2;
    for i in (0..=rect_size).step_by(grid_size as usize) {
        let _ =  draw_line(&mut vram, 0xffff00, cx, cy, 0, i);
        let _ =  draw_line(&mut vram, 0x00ffff, cx, cy, i, 0);
        let _ =  draw_line(&mut vram, 0xff00ff, cx, cy, rect_size, i);
        let _ =  draw_line(&mut vram, 0xffffff, cx, cy, i, rect_size);
    }

    let _ = draw_str(&mut vram, 0xffffff, &fonts, "Hello, World!", 500, 500);

    // println!("Hello, world!");
    loop {
        unsafe {
            asm!("hlt");
        }
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}