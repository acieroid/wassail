i32.const 1 ;; vstack: [x] <x = 1, x@b0 = 1, x@b1 = 0, x@b2 = 0, x@b3 = 0>
i32.const 2 ;; vstack: [y,x] <x = 1, x@b0 = 1, x@b1 = 0, x@b2 = 0, x@b3 = 0, y = 2, y@b0 = 2, y@b1 = 0, y@b2 = 0, y@b3 = 0>
i32.sub ;; vstack: [z], <z = x+y, z@b0 = T, z@b1 = T, z@b2 =T, Z@b3 = T>
i32.const 1024
i32.store ;; vstack: [], <z=x+y, a=1024...> M[a: z@b1, ...]
i32.load ;; vstack: [zz] [zz=x+y]
