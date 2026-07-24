pub fn Optional(comptime T: type) type {
    return union(enum) {
        value: T,
        none,

        pub fn is_some_and(self: @This(), fun: fn (value: T) bool) bool {
            return switch (self) {
                .value => |value| fun(value),
                .none => false,
            };
        }

        pub fn unwrap_or(self: @This(), default: T) T {
            return switch (self) {
                .value => |value| value,
                .none => default,
            };
        }
    };
}

pub fn Ref(comptime T: type) type {
    return union(enum) {
        owned: Owned,
        borrowed: Borrowed,

        const Self = @This();

        /// A value owned directly by this reference.
        pub const Owned = struct {
            value: T,

            pub fn init(value: T) @This() {
                return .{
                    .value = value,
                };
            }

            pub fn intoRef(self: @This()) Ref(T) {
                return .{
                    .owned = self,
                };
            }

            pub fn borrow(self: *const @This()) Borrowed {
                return .{ .ptr = &self.value };
            }
        };

        /// A reference to a value owned somewhere else.
        pub const Borrowed = struct {
            ptr: *const T,

            pub fn init(ptr: *const T) @This() {
                return .{
                    .ptr = ptr,
                };
            }

            pub fn get(self: @This()) *const T {
                return self.ptr;
            }

            pub fn intoRef(self: @This()) Ref(T) {
                return .{
                    .borrowed = self,
                };
            }
        };

        pub fn fromOwned(value: T) Self {
            return .{
                .owned = Owned.init(value),
            };
        }

        pub fn fromBorrowed(ptr: *const T) Self {
            return .{
                .borrowed = Borrowed.init(ptr),
            };
        }

        pub fn get(self: *const Self) *const T {
            return switch (self.*) {
                .owned => |*owned| owned.borrow().get(),
                .borrowed => |borrowed| borrowed.get(),
            };
        }

        pub fn isOwned(self: Self) bool {
            return switch (self) {
                .owned => true,
                .borrowed => false,
            };
        }

        pub fn isBorrowed(self: Self) bool {
            return !self.isOwned();
        }
    };
}
