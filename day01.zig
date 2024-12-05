const std = @import("std");

pub fn main() !void {
    std.debug.print("{d}\n", .{try solve(@embedFile("input01.txt"))});
}

fn solve(input: []const u8) !u32 {
    var lines = std.mem.split(u8, input, "\n");

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();

    const alloc = gpa.allocator();

    var lefts = std.ArrayList(u32).init(alloc);
    defer lefts.deinit();
    var rights = std.AutoHashMap(u32, void).init(alloc);
    defer rights.deinit();

    while (lines.next()) |line| {
        if (line.len == 0) break;

        const first = try std.fmt.parseInt(u32, line[0..5], 10);
        const second = try std.fmt.parseInt(u32, line[8..13], 10);

        try lefts.append(first);
        try rights.put(second, {});
    }

    var sum: u32 = 0;

    for (lefts.items) |left| {
        var count: u32 = 0;
        if (rights.get(left)) |_| {
            count += 1;
        }

        sum += left * count;
    }

    return sum;
}
