use std::io;

fn main() {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf);
    let addr: u32 = buf.trim().parse().unwrap();
    let radius: u32 = addr_radius(addr);
    println!("{:?}", distance_from_centre(addr, radius))
}

fn radius_end_addr(r: u32) -> u32 {
    (r*2 + 1).pow(2)
}

fn addr_radius(addr: u32) -> u32 {
    for x in 0.. {
        if radius_end_addr(x) >= addr {
            return x;
        }
    }
    panic!("at the disco")
}

fn distance_from_midpoint(addr: u32, radius: u32) -> u32 {
    if radius == 0 {
        return 0;
    }
    let lower_bound = (radius_end_addr(radius-1) + 1) as i32;
    let addr = addr as i32;
    let radius = radius as i32;
    (((addr - lower_bound + 1) % (2*radius)) - radius).abs() as u32
}

fn distance_from_centre(addr: u32, radius: u32) -> u32 {
    radius + distance_from_midpoint(addr, radius)
}
