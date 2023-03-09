const std = @import("std");
const windows = std.os.windows;
const user32 = std.os.windows.user32;
const c = @cImport({
    @cDefine("WIN32_LEAN_AND_MEAN", "1");
    @cInclude("windows.h");
    @cDefine("INITGUID", "1");
    @cInclude("devpkey.h");
    @cInclude("setupapi.h");
});
const cfg = struct {
    pub usingnamespace @cImport({
        @cInclude("cfg.h"); // for DN_.. enum
    });
    //from: @cInclude("devcaps.h"); // for CM_DEVCAP_.. enum
    const CM_DEVCAP_LOCKSUPPORTED = 0x00000001;
    const CM_DEVCAP_EJECTSUPPORTED = 0x00000002;
    const CM_DEVCAP_REMOVABLE = 0x00000004;
    const CM_DEVCAP_DOCKDEVICE = 0x00000008;
    const CM_DEVCAP_UNIQUEID = 0x00000010;
    const CM_DEVCAP_SILENTINSTALL = 0x00000020;
    const CM_DEVCAP_RAWDEVICEOK = 0x00000040;
    const CM_DEVCAP_SURPRISEREMOVALOK = 0x00000080;
    const CM_DEVCAP_HARDWAREDISABLED = 0x00000100;
    const CM_DEVCAP_NONDYNAMIC = 0x00000200;
    const CM_DEVCAP_SECUREDEVICE = 0x00000400;
};
const keyValue = struct { name: []const u8, val: u32 };
fn filterProps(src: anytype, comptime prefix: []const u8, comptime N: comptime_int) []keyValue {
    @setEvalBranchQuota(10000);
    var array: [N]keyValue = undefined;
    var i: u32 = 0;
    switch (@typeInfo(src)) {
        .Struct => |st| inline for (st.decls) |fld|
            if (std.mem.startsWith(u8, fld.name, prefix)) {
                array[i] = .{ .name = fld.name, .val = @field(src, fld.name) };
                i += 1;
            },
        else => unreachable,
    }
    return array[0..i];
}
const DN_PROPS = filterProps(cfg, "DN_", 40);
const CM_CAPS = filterProps(cfg, "CM_", 40);
fn printEnum(comptime arr: anytype, s: u32, stdout: anytype) void {
    inline for (arr) |fld|
        if ((fld.val & s) == fld.val)
            stdout.print("{s} ", .{fld.name}) catch unreachable;
}

fn getProp(devices: c.HANDLE, device: *c.SP_DEVINFO_DATA, prop: *const c.DEVPROPKEY, data: anytype) ?u32 {
    var ntype: c.DEVPROPTYPE = 0;
    var bytes: u32 = @sizeOf(@TypeOf(data.*));
    return if (c.SetupDiGetDevicePropertyW(devices, device, prop, &ntype, @ptrCast(*u8, data), bytes, &bytes, 0) != 0) bytes else null;
}
fn getPropStr(devices: c.HANDLE, device: *c.SP_DEVINFO_DATA, prop: *const c.DEVPROPKEY) ?[]u8 {
    var data: [1024]u16 = undefined;
    if (getProp(devices, device, prop, &data)) |bytes|
        return std.unicode.utf16leToUtf8Alloc(gpa, data[0..(bytes / 2 - 1)]) catch unreachable;
    return null;
}
fn getPropInt(devices: c.HANDLE, device: *c.SP_DEVINFO_DATA, prop: *const c.DEVPROPKEY) ?u32 {
    var data: u32 = 0;
    return if (getProp(devices, device, prop, &data) != null) data else null;
}

fn setDeviceState(devices: c.HANDLE, device: *c.SP_DEVINFO_DATA, state: c_ulong) bool {
    var params = std.mem.zeroInit(c.SP_PROPCHANGE_PARAMS, .{
        .ClassInstallHeader = .{ .cbSize = @sizeOf(c.SP_CLASSINSTALL_HEADER), .InstallFunction = c.DIF_PROPERTYCHANGE },
        .StateChange = state,
        .Scope = c.DICS_FLAG_CONFIGSPECIFIC,
        .HwProfile = 0,
    });
    return (c.SetupDiSetClassInstallParamsA(devices, device, &params.ClassInstallHeader, @sizeOf(c.SP_PROPCHANGE_PARAMS)) != 0) and
        (c.SetupDiCallClassInstaller(c.DIF_PROPERTYCHANGE, devices, device) != 0);
}

const DevNode = struct {
    id: []u8,
    name: ?[]u8,
    parentId: ?[]u8,
    children: std.ArrayList(*DevNode) = undefined,
    parent: ?*DevNode = null,
    candidateCached: ?bool = null,
    status: u32,
    caps: u32,
    device: c.SP_DEVINFO_DATA,
    fn printTree(self: *DevNode, stdout: anytype, depth: u32) !void {
        var msg: [1000]u8 = [_]u8{' '} ** 1000;
        try stdout.print("{s}{s:<60}: '{s}', \n", .{ msg[0 .. depth * 2], self.id, self.name orelse "<NULL>" });
        var iter = self.children.iterator(0);
        while (iter.next()) |child|
            try child.printTree(stdout, depth + 1);
    }
    fn candidateOrOwnedBy(self: *DevNode) bool {
        return self.candidate() or if (self.parent) |p| p.candidateOrOwnedBy() else false;
    }
    fn ownedByCandidate(self: *DevNode) bool {
        return if (self.parent) |p| p.candidateOrOwnedBy() else false;
    }
    fn print(self: *DevNode, stdout: anytype) !void {
        try stdout.print("{s:<60}: '{s}': ", .{ self.id, self.name orelse "<NULL>" });
        printEnum(DN_PROPS, self.status, stdout);
        try stdout.print(", ", .{});
        printEnum(CM_CAPS, self.caps, stdout);
        try stdout.print("\n", .{});
    }
    fn candidate(self: *DevNode) bool {
        if (self.candidateCached == null) {
            self.candidateCached = !std.mem.startsWith(u8, self.id, "SWD\\") and !std.mem.startsWith(u8, self.id, "SW\\") and // skip softdevices
                !std.mem.startsWith(u8, self.id, "HID\\") and !std.mem.startsWith(u8, self.id, "DISPLAY\\") and // and input/output
                self.caps & cfg.CM_DEVCAP_SURPRISEREMOVALOK == cfg.CM_DEVCAP_SURPRISEREMOVALOK and // only removable
                (showHidden or self.status & cfg.DN_REMOVABLE == cfg.DN_REMOVABLE);
            //std.mem.startsWith(u8, id, "PCI\\VEN_8086&DEV_15DA");
        }
        return self.candidateCached.?;
    }
};

// HGDIBITMAP alignment is fucked in translated c-code, so use HGDIOBJ
pub extern fn SetMenuItemBitmaps(hMenu: c.HMENU, uPosition: c.UINT, uFlags: c.UINT, hBitmapUnchecked: c.HGDIOBJ, hBitmapChecked: c.HGDIOBJ) c.WINBOOL;
const MF_GRAYED = 0x0001;
const MF_SEPARATOR = 0x0800;
const MF_BYPOSITION = 0x0400;
const MF_BYCOMMAND = 0x0000;
const TPM_RETURNCMD = 0x0100;
const TPM_NONOTIFY = 0x0080;
fn bitmapFromGuid(guid: *c.GUID, dim: bool) c.HGDIOBJ {
    var hDC = c.CreateCompatibleDC(0);
    defer _ = c.DeleteDC(hDC);
    var width = c.GetSystemMetrics(c.SM_CXSMICON);
    var height = c.GetSystemMetrics(c.SM_CYSMICON);
    if (hDC != null) {
        var bitmapInfo = std.mem.zeroInit(c.BITMAPINFO, .{
            .bmiHeader = std.mem.zeroInit(c.BITMAPINFOHEADER, .{ .biSize = @sizeOf(c.BITMAPINFOHEADER), .biWidth = width, .biHeight = height, .biPlanes = 1, .biBitCount = 32 }),
        });
        var hDCBitmap = c.GetDC(null);
        var hBitmap = c.CreateDIBSection(hDCBitmap, &bitmapInfo, c.DIB_RGB_COLORS, null, null, 0);
        _ = c.ReleaseDC(null, hDCBitmap);
        if (hBitmap != null) {
            var iid: c.INT = 0;
            var hBitmapOld = c.SelectObject(hDC, hBitmap);
            if (hBitmapOld != null and c.SetupDiGetClassImageIndex(&imgList, guid, &iid) != 0 and //
                c.ImageList_DrawEx(imgList.ImageList, iid, hDC, 0, 0, width, height, 0, 0, @bitCast(c_uint, c.ILD_TRANSPARENT | if (dim) c.ILD_BLEND50 else 0)) != 0)
                return c.SelectObject(hDC, hBitmapOld);
            _ = c.DeleteObject(hBitmap);
        }
    }
    return null;
}

fn appendMenu(menu: c.HMENU, uFlags: c.UINT, idGen: *u32, actId: usize, img: ?c.HGDIOBJ, comptime fmt: []const u8, args: anytype) void {
    const maxW = 1024;
    var buff: [maxW]u8 = undefined;
    const title = std.fmt.bufPrint(&buff, fmt, args) catch return;
    var wide = std.unicode.utf8ToUtf16LeWithNull(gpa, title) catch unreachable;
    defer gpa.free(wide);
    _ = c.AppendMenuW(menu, uFlags, actId, wide.ptr);
    if (img) |image|
        _ = SetMenuItemBitmaps(menu, idGen.*, MF_BYPOSITION, image, image);
    idGen.* += 1;
}

fn appendSubtree(menu: c.HMENU, idGen: *u32, depth: u32, children: []*DevNode) void {
    for (children) |child| {
        if (!showHidden and child.status == 0)
            continue;
        //var bmp = bitmapFromGuid(&child.device.ClassGuid, child.status == 0);
        appendMenu(menu, MF_GRAYED, idGen, 0, null, "{s:[3]}- {s} {s}", .{ "", if (child.status == 0) "⏏" else "", child.name orelse "<NULL>", depth * 3 });
        appendSubtree(menu, idGen, depth + 1, child.children.items);
    }
}

fn askUser(variants: []*DevNode) ?*DevNode {
    const hInstance = c.GetModuleHandleW(null);
    const hwnd = c.CreateWindowExA(user32.WS_EX_TOOLWINDOW, "button", null, user32.WS_OVERLAPPED, 0, 0, 0, 0, 0, 0, hInstance, null);
    defer _ = c.DestroyWindow(hwnd);
    const menu = c.CreatePopupMenu();
    defer _ = c.DestroyMenu(menu);
    var idGen: u32 = 0;
    if (variants.len == 0) {
        appendMenu(menu, MF_GRAYED, &idGen, 0, null, "No removable devices found", .{});
    } else {
        for (variants) |val, idx| {
            if (idx > 0)
                _ = appendMenu(menu, MF_SEPARATOR, &idGen, 0, null, "", .{});
            var bmp = bitmapFromGuid(&val.device.ClassGuid, val.status == 0);
            appendMenu(menu, 0, &idGen, idx + 1, bmp, "{s}{s}", .{ if (val.status == 0) "" else "Eject ", val.name orelse "<NULL>" });
            appendSubtree(menu, &idGen, 1, val.children.items);
        }
    }
    _ = c.SetForegroundWindow(hwnd);
    var pt: c.POINT = undefined;
    _ = c.GetCursorPos(&pt);
    var x = c.TrackPopupMenu(menu, TPM_RETURNCMD | TPM_NONOTIFY, pt.x, pt.y, 0, hwnd, null);
    return if (x > 0) variants[@intCast(u32, x - 1)] else null;
}

var imgList = c.SP_CLASSIMAGELIST_DATA{ .cbSize = @sizeOf(c.SP_CLASSIMAGELIST_DATA), .ImageList = null, .Reserved = 0 };
var showHidden = false;
var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
const gpa = general_purpose_allocator.allocator();

pub fn main() !void {
    showHidden = c.GetKeyState(c.VK_SHIFT) & @bitCast(i16, @as(u16, 0x8000)) != 0;
    _ = c.SetupDiGetClassImageList(&imgList);
    var allDevs = std.ArrayList(DevNode).init(gpa);
    var devices = c.SetupDiGetClassDevsA(null, null, null, c.DIGCF_ALLCLASSES);
    defer _ = c.SetupDiDestroyDeviceInfoList(devices);
    { // list all devices
        var device = std.mem.zeroInit(c.SP_DEVINFO_DATA, .{ .cbSize = @sizeOf(c.SP_DEVINFO_DATA) });
        var i: u32 = 0;
        while (c.SetupDiEnumDeviceInfo(devices, i, &device) != 0) : (i += 1)
            if (getPropStr(devices, &device, &c.DEVPKEY_Device_InstanceId)) |key|
                try allDevs.append(.{
                    .id = key,
                    .name = getPropStr(devices, &device, &c.DEVPKEY_NAME),
                    .parentId = getPropStr(devices, &device, &c.DEVPKEY_Device_Parent),
                    .status = getPropInt(devices, &device, &c.DEVPKEY_Device_DevNodeStatus) orelse 0,
                    .caps = getPropInt(devices, &device, &c.DEVPKEY_Device_Capabilities) orelse 0,
                    .device = device,
                    .children = std.ArrayList(*DevNode).init(gpa),
                });
    }
    { // assemble the tree
        var devsCache = std.StringHashMap(*DevNode).init(gpa);
        for (allDevs.items) |*val|
            try devsCache.put(val.id, val);
        for (allDevs.items) |*val| {
            if (val.parentId) |p| {
                if (devsCache.getEntry(p)) |pEntry| {
                    try pEntry.value_ptr.*.children.append(val);
                    val.parent = pEntry.value_ptr.*;
                }
            }
        }
        devsCache.deinit();
    }
    var variants = std.ArrayList(*DevNode).init(gpa);
    for (allDevs.items) |*val|
        if (val.candidate() and !val.ownedByCandidate())
            try variants.append(val);
    if (askUser(variants.items)) |dev| {
        _ = c.AllocConsole();
        const stdout = std.io.getStdOut().writer();
        try stdout.print("Please wait while disabling '{s}'...", .{dev.id});
        if (!setDeviceState(devices, &dev.device, c.DICS_STOP)) { //c.DICS_DISABLE
            try stdout.print("Failed to disable the device, error: {X}\n", .{c.GetLastError()});
            return;
        }
        try stdout.print("\n\nNow, REMOVE the device\n", .{});
        while (dev.status != 0) {
            try stdout.print(".", .{});
            std.time.sleep(5e8);
            dev.status = getPropInt(devices, &dev.device, &c.DEVPKEY_Device_DevNodeStatus) orelse 0;
        }
        try stdout.print("\nRe-enabling device...", .{});
        if (!setDeviceState(devices, &dev.device, c.DICS_START)) { //c.DICS_ENABLE
            try stdout.print("Failed to re-enable the device, error: {X}\n", .{c.GetLastError()});
            return;
        }
        try stdout.print("\nDone!\n", .{});
    }
}
