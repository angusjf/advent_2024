const std = @import("std");

pub fn main() !void {
    std.debug.print("{d}\n", .{try solve(@embedFile("input09.txt"))});
}

fn solve(input: []const u8) !u64 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();

    const alloc = gpa.allocator();

    var disk = std.ArrayList(?u32).init(alloc);
    defer disk.deinit();

    {
        var i: u32 = 0;
        var empty = false;

        for (input) |c| {
            if (c == 10) break;
            const cc: u8 = c - '0';
            for (0..cc) |_| {
                try disk.append(if (empty) null else i);
            }
            if (!empty) {
                i += 1;
            }
            empty = !empty;
        }
    }

    try defrag(disk.items);

    return checksum(disk.items);
}

fn defrag(disk: []?u32) !void {
    var right_ptr: usize = disk.len - 1;

    while (disk[right_ptr] == null) {
        right_ptr -= 1;
    }

    var id = disk[right_ptr].?;

    id_loop: while (true) {
        while (disk[right_ptr] != id) {
            right_ptr -= 1;
        }
        var len: u32 = 0;
        while (len <= right_ptr and right_ptr - len >= 0 and disk[right_ptr - len] == id) {
            len += 1;
        }

        var cursor: usize = 0;

        while (true) {
            if (disk[cursor] == id) {
                break;
            }

            var gap_size: usize = 0;

            while (disk[cursor + gap_size] == null) {
                gap_size += 1;
                if (cursor + gap_size + 2 >= disk.len) {
                    id -= 1;
                    continue :id_loop;
                }
            }

            if (gap_size >= len) {
                for (0..len) |i| {
                    disk[cursor + i] = disk[right_ptr - i];
                    disk[right_ptr - i] = null;
                }
                break;
            } else {
                cursor += 1;
                if (cursor + gap_size + 2 >= disk.len) {
                    if (id == 0) {
                        break :id_loop;
                    } else {
                        id -= 1;
                        continue :id_loop;
                    }
                }
            }
        }

        if (id == 0) {
            break;
        } else {
            id -= 1;
        }
    }
}

fn checksum(disk: []?u32) u64 {
    var result: u64 = 0;

    for (0.., disk) |i, n| {
        if (n) |nv| {
            result += i * nv;
        }
    }

    return result;
}

test "day09" {
    try std.testing.expect(try solve("2333133121414131402") == 2858);
}
