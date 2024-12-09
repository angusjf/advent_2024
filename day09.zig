const std = @import("std");

pub fn main() !void {
    std.debug.print("{d}\n", .{try solve(@embedFile("input09.txt"))});
    //// std.debug.print("{d}\n", .{try solve(@embedFile("test09.txt"))});
}

fn solve(input: []const u8) !u64 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const alloc = gpa.allocator();

    var ns = std.ArrayList(?u32).init(alloc);

    // 2333133121414131402
    // 00 ... 111 ... 2 ... 333.44 . 5555.6666 . 777.888899
    {
        var i: u32 = 0;
        var empty = false;

        for (input) |c| {
            if (c == 10) break;
            const cc: u8 = c - '0';
            for (0..cc) |_| {
                try ns.append(if (empty) null else i);
            }
            if (!empty) {
                i += 1;
            }
            empty = !empty;
        }
    }

    var empty_ptr: usize = 0;

    while (ns.items[empty_ptr] != null) {
        empty_ptr += 1;
    }

    var right_ptr: usize = ns.items.len - 1;

    while (ns.items[right_ptr] == null) {
        right_ptr -= 1;
    }

    var id = ns.items[right_ptr].?;

    id_loop: while (true) {
        //std.debug.print("id={any}\n", .{id});

        while (ns.items[right_ptr] != id) {
            right_ptr -= 1;
        }
        var len: u32 = 0;
        while (len <= right_ptr and right_ptr - len >= 0 and ns.items[right_ptr - len] == id) {
            len += 1;
        }

        var cursor: usize = 0;

        while (true) {
            //std.debug.print("  looking for gap with size len={any} cursor={any}\n", .{ len, cursor });
            if (ns.items[cursor] == id) {
                //std.debug.print("  went too far cursor={any} id={any}\n", .{ cursor, id });

                break;
            }

            var gap_size: usize = 0;

            while (ns.items[cursor + gap_size] == null) {
                gap_size += 1;
                //std.debug.print("    found gap gap_size={any}\n", .{gap_size});
                if (cursor + gap_size + 2 >= ns.items.len) {
                    id -= 1;
                    continue :id_loop;
                }
            }

            if (gap_size >= len) {
                //std.debug.print("   found gap size={any} from={any}\n", .{ gap_size, cursor });

                for (0..len) |i| {
                    ns.items[cursor + i] = ns.items[right_ptr - i];
                    ns.items[right_ptr - i] = null;
                }
                break;
            } else {
                cursor += 1;
                if (cursor + gap_size + 2 >= ns.items.len) {
                    if (id == 0) {
                        break :id_loop;
                    } else {
                        id -= 1;
                        continue :id_loop;
                    }
                }
            }
        }

        //std.debug.print("  {any}\n", .{ns.items});

        if (id == 0) {
            break;
        } else {
            id -= 1;
        }
    }

    return checksum(ns.items);
}

fn checksum(ns: []?u32) u64 {
    var result: u64 = 0;

    for (0.., ns) |i, n| {
        if (n) |nv| {
            result += i * nv;
        }
    }

    return result;
}
