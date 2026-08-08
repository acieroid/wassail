(module $TR1486.wasm
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32 i64 i32) (result i32)))
  (type (;2;) (func (param i32 i64 i64 i32) (result i32)))
  (type (;3;) (func (param i32 i64 i64) (result i32)))
  (type (;4;) (func (param i32) (result i32)))
  (type (;5;) (func (param i32 i64) (result i32)))
  (type (;6;) (func (param i32 i32 i32 i64 i32) (result i32)))
  (type (;7;) (func (param i32 i32 i32) (result i32)))
  (type (;8;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;9;) (func (param i32 i64 i32 i32) (result i32)))
  (type (;10;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;11;) (func (param i32 i32 i32 i32 i64 i64 i32) (result i32)))
  (type (;12;) (func (param i32 i32 i32 i32 i32 i32 i32) (result i32)))
  (type (;13;) (func (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
  (type (;14;) (func (param i32 i32 i32 i32 i32 i32) (result i32)))
  (type (;15;) (func (param i32)))
  (type (;16;) (func (result i32)))
  (type (;17;) (func))
  (type (;18;) (func (param i32 i32 i32 i64 i64 i32) (result i32)))
  (type (;19;) (func (param i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "args_get" (func $__imported_wasi_snapshot_preview1_args_get (type 0)))
  (import "wasi_snapshot_preview1" "args_sizes_get" (func $__imported_wasi_snapshot_preview1_args_sizes_get (type 0)))
  (import "wasi_snapshot_preview1" "environ_get" (func $__imported_wasi_snapshot_preview1_environ_get (type 0)))
  (import "wasi_snapshot_preview1" "environ_sizes_get" (func $__imported_wasi_snapshot_preview1_environ_sizes_get (type 0)))
  (import "wasi_snapshot_preview1" "clock_res_get" (func $__imported_wasi_snapshot_preview1_clock_res_get (type 0)))
  (import "wasi_snapshot_preview1" "clock_time_get" (func $__imported_wasi_snapshot_preview1_clock_time_get (type 1)))
  (import "wasi_snapshot_preview1" "fd_advise" (func $__imported_wasi_snapshot_preview1_fd_advise (type 2)))
  (import "wasi_snapshot_preview1" "fd_allocate" (func $__imported_wasi_snapshot_preview1_fd_allocate (type 3)))
  (import "wasi_snapshot_preview1" "fd_close" (func $__imported_wasi_snapshot_preview1_fd_close (type 4)))
  (import "wasi_snapshot_preview1" "fd_datasync" (func $__imported_wasi_snapshot_preview1_fd_datasync (type 4)))
  (import "wasi_snapshot_preview1" "fd_fdstat_get" (func $__imported_wasi_snapshot_preview1_fd_fdstat_get (type 0)))
  (import "wasi_snapshot_preview1" "fd_fdstat_set_flags" (func $__imported_wasi_snapshot_preview1_fd_fdstat_set_flags (type 0)))
  (import "wasi_snapshot_preview1" "fd_fdstat_set_rights" (func $__imported_wasi_snapshot_preview1_fd_fdstat_set_rights (type 3)))
  (import "wasi_snapshot_preview1" "fd_filestat_get" (func $__imported_wasi_snapshot_preview1_fd_filestat_get (type 0)))
  (import "wasi_snapshot_preview1" "fd_filestat_set_size" (func $__imported_wasi_snapshot_preview1_fd_filestat_set_size (type 5)))
  (import "wasi_snapshot_preview1" "fd_filestat_set_times" (func $__imported_wasi_snapshot_preview1_fd_filestat_set_times (type 2)))
  (import "wasi_snapshot_preview1" "fd_pread" (func $__imported_wasi_snapshot_preview1_fd_pread (type 6)))
  (import "wasi_snapshot_preview1" "fd_prestat_get" (func $__imported_wasi_snapshot_preview1_fd_prestat_get (type 0)))
  (import "wasi_snapshot_preview1" "fd_prestat_dir_name" (func $__imported_wasi_snapshot_preview1_fd_prestat_dir_name (type 7)))
  (import "wasi_snapshot_preview1" "fd_pwrite" (func $__imported_wasi_snapshot_preview1_fd_pwrite (type 6)))
  (import "wasi_snapshot_preview1" "fd_read" (func $__imported_wasi_snapshot_preview1_fd_read (type 8)))
  (import "wasi_snapshot_preview1" "fd_readdir" (func $__imported_wasi_snapshot_preview1_fd_readdir (type 6)))
  (import "wasi_snapshot_preview1" "fd_renumber" (func $__imported_wasi_snapshot_preview1_fd_renumber (type 0)))
  (import "wasi_snapshot_preview1" "fd_seek" (func $__imported_wasi_snapshot_preview1_fd_seek (type 9)))
  (import "wasi_snapshot_preview1" "fd_sync" (func $__imported_wasi_snapshot_preview1_fd_sync (type 4)))
  (import "wasi_snapshot_preview1" "fd_tell" (func $__imported_wasi_snapshot_preview1_fd_tell (type 0)))
  (import "wasi_snapshot_preview1" "fd_write" (func $__imported_wasi_snapshot_preview1_fd_write (type 8)))
  (import "wasi_snapshot_preview1" "path_create_directory" (func $__imported_wasi_snapshot_preview1_path_create_directory (type 7)))
  (import "wasi_snapshot_preview1" "path_filestat_get" (func $__imported_wasi_snapshot_preview1_path_filestat_get (type 10)))
  (import "wasi_snapshot_preview1" "path_filestat_set_times" (func $__imported_wasi_snapshot_preview1_path_filestat_set_times (type 11)))
  (import "wasi_snapshot_preview1" "path_link" (func $__imported_wasi_snapshot_preview1_path_link (type 12)))
  (import "wasi_snapshot_preview1" "path_open" (func $__imported_wasi_snapshot_preview1_path_open (type 13)))
  (import "wasi_snapshot_preview1" "path_readlink" (func $__imported_wasi_snapshot_preview1_path_readlink (type 14)))
  (import "wasi_snapshot_preview1" "path_remove_directory" (func $__imported_wasi_snapshot_preview1_path_remove_directory (type 7)))
  (import "wasi_snapshot_preview1" "path_rename" (func $__imported_wasi_snapshot_preview1_path_rename (type 14)))
  (import "wasi_snapshot_preview1" "path_symlink" (func $__imported_wasi_snapshot_preview1_path_symlink (type 10)))
  (import "wasi_snapshot_preview1" "path_unlink_file" (func $__imported_wasi_snapshot_preview1_path_unlink_file (type 7)))
  (import "wasi_snapshot_preview1" "poll_oneoff" (func $__imported_wasi_snapshot_preview1_poll_oneoff (type 8)))
  (import "wasi_snapshot_preview1" "proc_exit" (func $__imported_wasi_snapshot_preview1_proc_exit (type 15)))
  (import "wasi_snapshot_preview1" "sched_yield" (func $__imported_wasi_snapshot_preview1_sched_yield (type 16)))
  (import "wasi_snapshot_preview1" "random_get" (func $__imported_wasi_snapshot_preview1_random_get (type 0)))
  (import "wasi_snapshot_preview1" "sock_accept" (func $__imported_wasi_snapshot_preview1_sock_accept (type 7)))
  (import "wasi_snapshot_preview1" "sock_recv" (func $__imported_wasi_snapshot_preview1_sock_recv (type 14)))
  (import "wasi_snapshot_preview1" "sock_send" (func $__imported_wasi_snapshot_preview1_sock_send (type 10)))
  (import "wasi_snapshot_preview1" "sock_shutdown" (func $__imported_wasi_snapshot_preview1_sock_shutdown (type 0)))
  (func $__wasm_call_ctors (type 17))
  (func $_start (type 17)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        global.get $GOT.data.internal.__memory_base
        i32.const 1028
        i32.add
        i32.load
        br_if 0 (;@2;)
        global.get $GOT.data.internal.__memory_base
        i32.const 1028
        i32.add
        i32.const 1
        i32.store
        call $__wasm_call_ctors
        call $__original_main
        local.set 0
        call $__wasm_call_dtors
        local.get 0
        br_if 1 (;@1;)
        return
      end
      unreachable
    end
    local.get 0
    call $__wasi_proc_exit
    unreachable)
  (func $__original_main (type 16) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    local.set 0
    i32.const 80
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    i32.const 0
    local.set 3
    local.get 2
    local.get 3
    i32.store offset=76
    i32.const 16
    local.set 4
    local.get 2
    local.get 4
    i32.add
    local.set 5
    local.get 5
    local.set 6
    local.get 2
    local.get 6
    i32.store offset=72
    local.get 2
    i32.load offset=72
    local.set 7
    local.get 2
    local.get 7
    i32.store offset=12
    i32.const 16
    local.set 8
    local.get 2
    local.get 8
    i32.add
    local.set 9
    local.get 9
    local.set 10
    i32.const 20
    local.set 11
    local.get 10
    local.get 11
    i32.add
    local.set 12
    local.get 2
    local.get 12
    i32.store offset=68
    i32.const 0
    local.set 13
    local.get 2
    local.get 13
    i32.store offset=8
    block  ;; label = @1
      loop  ;; label = @2
        local.get 2
        i32.load offset=8
        local.set 14
        i32.const 5
        local.set 15
        local.get 14
        local.get 15
        i32.lt_s
        local.set 16
        i32.const 1
        local.set 17
        local.get 16
        local.get 17
        i32.and
        local.set 18
        local.get 18
        i32.eqz
        br_if 1 (;@1;)
        i32.const 0
        local.set 19
        local.get 19
        i32.load offset=1032
        local.set 20
        local.get 2
        i32.load offset=72
        local.set 21
        local.get 21
        local.get 20
        i32.store
        i32.const 0
        local.set 22
        local.get 22
        i32.load offset=1024
        local.set 23
        local.get 2
        i32.load offset=68
        local.set 24
        local.get 24
        local.get 23
        i32.store
        local.get 2
        i32.load offset=72
        local.set 25
        i32.const 4
        local.set 26
        local.get 25
        local.get 26
        i32.add
        local.set 27
        local.get 2
        local.get 27
        i32.store offset=72
        local.get 2
        i32.load offset=68
        local.set 28
        i32.const 4
        local.set 29
        local.get 28
        local.get 29
        i32.add
        local.set 30
        local.get 2
        local.get 30
        i32.store offset=68
        local.get 2
        i32.load offset=8
        local.set 31
        i32.const 1
        local.set 32
        local.get 31
        local.get 32
        i32.add
        local.set 33
        local.get 2
        local.get 33
        i32.store offset=8
        br 0 (;@2;)
      end
    end
    local.get 2
    i32.load offset=12
    local.set 34
    local.get 34
    i32.load
    local.set 35
    local.get 35
    return)
  (func $main (type 0) (param i32 i32) (result i32)
    (local i32)
    call $__original_main
    local.set 2
    local.get 2
    return)
  (func $__wasi_args_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_args_get
    i32.const 65535
    i32.and)
  (func $__wasi_args_sizes_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_args_sizes_get
    i32.const 65535
    i32.and)
  (func $__wasi_environ_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_environ_get
    i32.const 65535
    i32.and)
  (func $__wasi_environ_sizes_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_environ_sizes_get
    i32.const 65535
    i32.and)
  (func $__wasi_clock_res_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_clock_res_get
    i32.const 65535
    i32.and)
  (func $__wasi_clock_time_get (type 1) (param i32 i64 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    call $__imported_wasi_snapshot_preview1_clock_time_get
    i32.const 65535
    i32.and)
  (func $__wasi_fd_advise (type 2) (param i32 i64 i64 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $__imported_wasi_snapshot_preview1_fd_advise
    i32.const 65535
    i32.and)
  (func $__wasi_fd_allocate (type 3) (param i32 i64 i64) (result i32)
    local.get 0
    local.get 1
    local.get 2
    call $__imported_wasi_snapshot_preview1_fd_allocate
    i32.const 65535
    i32.and)
  (func $__wasi_fd_close (type 4) (param i32) (result i32)
    local.get 0
    call $__imported_wasi_snapshot_preview1_fd_close
    i32.const 65535
    i32.and)
  (func $__wasi_fd_datasync (type 4) (param i32) (result i32)
    local.get 0
    call $__imported_wasi_snapshot_preview1_fd_datasync
    i32.const 65535
    i32.and)
  (func $__wasi_fd_fdstat_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_fd_fdstat_get
    i32.const 65535
    i32.and)
  (func $__wasi_fd_fdstat_set_flags (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_fd_fdstat_set_flags
    i32.const 65535
    i32.and)
  (func $__wasi_fd_fdstat_set_rights (type 3) (param i32 i64 i64) (result i32)
    local.get 0
    local.get 1
    local.get 2
    call $__imported_wasi_snapshot_preview1_fd_fdstat_set_rights
    i32.const 65535
    i32.and)
  (func $__wasi_fd_filestat_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_fd_filestat_get
    i32.const 65535
    i32.and)
  (func $__wasi_fd_filestat_set_size (type 5) (param i32 i64) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_fd_filestat_set_size
    i32.const 65535
    i32.and)
  (func $__wasi_fd_filestat_set_times (type 2) (param i32 i64 i64 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $__imported_wasi_snapshot_preview1_fd_filestat_set_times
    i32.const 65535
    i32.and)
  (func $__wasi_fd_pread (type 6) (param i32 i32 i32 i64 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    call $__imported_wasi_snapshot_preview1_fd_pread
    i32.const 65535
    i32.and)
  (func $__wasi_fd_prestat_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_fd_prestat_get
    i32.const 65535
    i32.and)
  (func $__wasi_fd_prestat_dir_name (type 7) (param i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    call $__imported_wasi_snapshot_preview1_fd_prestat_dir_name
    i32.const 65535
    i32.and)
  (func $__wasi_fd_pwrite (type 6) (param i32 i32 i32 i64 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    call $__imported_wasi_snapshot_preview1_fd_pwrite
    i32.const 65535
    i32.and)
  (func $__wasi_fd_read (type 8) (param i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $__imported_wasi_snapshot_preview1_fd_read
    i32.const 65535
    i32.and)
  (func $__wasi_fd_readdir (type 6) (param i32 i32 i32 i64 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    call $__imported_wasi_snapshot_preview1_fd_readdir
    i32.const 65535
    i32.and)
  (func $__wasi_fd_renumber (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_fd_renumber
    i32.const 65535
    i32.and)
  (func $__wasi_fd_seek (type 9) (param i32 i64 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $__imported_wasi_snapshot_preview1_fd_seek
    i32.const 65535
    i32.and)
  (func $__wasi_fd_sync (type 4) (param i32) (result i32)
    local.get 0
    call $__imported_wasi_snapshot_preview1_fd_sync
    i32.const 65535
    i32.and)
  (func $__wasi_fd_tell (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_fd_tell
    i32.const 65535
    i32.and)
  (func $__wasi_fd_write (type 8) (param i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $__imported_wasi_snapshot_preview1_fd_write
    i32.const 65535
    i32.and)
  (func $__wasi_path_create_directory (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 1
    call $strlen
    call $__imported_wasi_snapshot_preview1_path_create_directory
    i32.const 65535
    i32.and)
  (func $__wasi_path_filestat_get (type 8) (param i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 2
    call $strlen
    local.get 3
    call $__imported_wasi_snapshot_preview1_path_filestat_get
    i32.const 65535
    i32.and)
  (func $__wasi_path_filestat_set_times (type 18) (param i32 i32 i32 i64 i64 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 2
    call $strlen
    local.get 3
    local.get 4
    local.get 5
    call $__imported_wasi_snapshot_preview1_path_filestat_set_times
    i32.const 65535
    i32.and)
  (func $__wasi_path_link (type 10) (param i32 i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 2
    call $strlen
    local.get 3
    local.get 4
    local.get 4
    call $strlen
    call $__imported_wasi_snapshot_preview1_path_link
    i32.const 65535
    i32.and)
  (func $__wasi_path_open (type 19) (param i32 i32 i32 i32 i64 i64 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 2
    call $strlen
    local.get 3
    local.get 4
    local.get 5
    local.get 6
    local.get 7
    call $__imported_wasi_snapshot_preview1_path_open
    i32.const 65535
    i32.and)
  (func $__wasi_path_readlink (type 10) (param i32 i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 1
    call $strlen
    local.get 2
    local.get 3
    local.get 4
    call $__imported_wasi_snapshot_preview1_path_readlink
    i32.const 65535
    i32.and)
  (func $__wasi_path_remove_directory (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 1
    call $strlen
    call $__imported_wasi_snapshot_preview1_path_remove_directory
    i32.const 65535
    i32.and)
  (func $__wasi_path_rename (type 8) (param i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 1
    call $strlen
    local.get 2
    local.get 3
    local.get 3
    call $strlen
    call $__imported_wasi_snapshot_preview1_path_rename
    i32.const 65535
    i32.and)
  (func $__wasi_path_symlink (type 7) (param i32 i32 i32) (result i32)
    local.get 0
    local.get 0
    call $strlen
    local.get 1
    local.get 2
    local.get 2
    call $strlen
    call $__imported_wasi_snapshot_preview1_path_symlink
    i32.const 65535
    i32.and)
  (func $__wasi_path_unlink_file (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 1
    call $strlen
    call $__imported_wasi_snapshot_preview1_path_unlink_file
    i32.const 65535
    i32.and)
  (func $__wasi_poll_oneoff (type 8) (param i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $__imported_wasi_snapshot_preview1_poll_oneoff
    i32.const 65535
    i32.and)
  (func $__wasi_proc_exit (type 15) (param i32)
    local.get 0
    call $__imported_wasi_snapshot_preview1_proc_exit
    unreachable)
  (func $__wasi_sched_yield (type 16) (result i32)
    call $__imported_wasi_snapshot_preview1_sched_yield
    i32.const 65535
    i32.and)
  (func $__wasi_random_get (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_random_get
    i32.const 65535
    i32.and)
  (func $__wasi_sock_accept (type 7) (param i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    call $__imported_wasi_snapshot_preview1_sock_accept
    i32.const 65535
    i32.and)
  (func $__wasi_sock_recv (type 14) (param i32 i32 i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    local.get 5
    call $__imported_wasi_snapshot_preview1_sock_recv
    i32.const 65535
    i32.and)
  (func $__wasi_sock_send (type 10) (param i32 i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    call $__imported_wasi_snapshot_preview1_sock_send
    i32.const 65535
    i32.and)
  (func $__wasi_sock_shutdown (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_sock_shutdown
    i32.const 65535
    i32.and)
  (func $_Exit (type 15) (param i32)
    local.get 0
    call $__wasi_proc_exit
    unreachable)
  (func $dummy (type 17))
  (func $__wasm_call_dtors (type 17)
    call $dummy
    call $dummy)
  (func $exit (type 15) (param i32)
    call $dummy
    call $dummy
    local.get 0
    call $_Exit
    unreachable)
  (func $strlen (type 4) (param i32) (result i32)
    (local i32 i32 i32)
    local.get 0
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 0
          i32.load8_u
          br_if 0 (;@3;)
          local.get 0
          local.get 0
          i32.sub
          return
        end
        local.get 0
        i32.const 1
        i32.add
        local.tee 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 2
        i32.add
        local.tee 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 3
        i32.add
        local.tee 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 4
        i32.add
        local.tee 1
        i32.const 3
        i32.and
        br_if 1 (;@1;)
      end
      local.get 1
      i32.const -4
      i32.add
      local.set 2
      local.get 1
      i32.const -5
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 1
        i32.const 4
        i32.add
        local.set 1
        i32.const 16843008
        local.get 2
        i32.const 4
        i32.add
        local.tee 2
        i32.load
        local.tee 3
        i32.sub
        local.get 3
        i32.or
        i32.const -2139062144
        i32.and
        i32.const -2139062144
        i32.eq
        br_if 0 (;@2;)
      end
      loop  ;; label = @2
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 2
        i32.load8_u
        local.set 3
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        local.get 3
        br_if 0 (;@2;)
      end
    end
    local.get 1
    local.get 0
    i32.sub)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global $__stack_pointer (mut i32) (i32.const 66576))
  (global $GOT.data.internal.__memory_base i32 (i32.const 0))
  (global (;2;) i32 (i32.const 0))
  (global (;3;) i32 (i32.const 1032))
  (global (;4;) i32 (i32.const 1024))
  (global (;5;) i32 (i32.const 1024))
  (global (;6;) i32 (i32.const 1036))
  (global (;7;) i32 (i32.const 1040))
  (global (;8;) i32 (i32.const 66576))
  (global (;9;) i32 (i32.const 1024))
  (global (;10;) i32 (i32.const 66576))
  (global (;11;) i32 (i32.const 131072))
  (global (;12;) i32 (i32.const 1))
  (export "memory" (memory 0))
  (export "__wasm_call_ctors" (func $__wasm_call_ctors))
  (export "_start" (func $_start))
  (export "__memory_base" (global 2))
  (export "__main_void" (func $__original_main))
  (export "__wasm_call_dtors" (func $__wasm_call_dtors))
  (export "__wasi_proc_exit" (func $__wasi_proc_exit))
  (export "__indirect_function_table" (table 0))
  (export "__original_main" (func $__original_main))
  (export "part1Value" (global 3))
  (export "part2Value" (global 4))
  (export "main" (func $main))
  (export "_Exit" (func $_Exit))
  (export "_exit" (func $_Exit))
  (export "__wasi_args_get" (func $__wasi_args_get))
  (export "__wasi_args_sizes_get" (func $__wasi_args_sizes_get))
  (export "__wasi_environ_get" (func $__wasi_environ_get))
  (export "__wasi_environ_sizes_get" (func $__wasi_environ_sizes_get))
  (export "__wasi_clock_res_get" (func $__wasi_clock_res_get))
  (export "__wasi_clock_time_get" (func $__wasi_clock_time_get))
  (export "__wasi_fd_advise" (func $__wasi_fd_advise))
  (export "__wasi_fd_allocate" (func $__wasi_fd_allocate))
  (export "__wasi_fd_close" (func $__wasi_fd_close))
  (export "__wasi_fd_datasync" (func $__wasi_fd_datasync))
  (export "__wasi_fd_fdstat_get" (func $__wasi_fd_fdstat_get))
  (export "__wasi_fd_fdstat_set_flags" (func $__wasi_fd_fdstat_set_flags))
  (export "__wasi_fd_fdstat_set_rights" (func $__wasi_fd_fdstat_set_rights))
  (export "__wasi_fd_filestat_get" (func $__wasi_fd_filestat_get))
  (export "__wasi_fd_filestat_set_size" (func $__wasi_fd_filestat_set_size))
  (export "__wasi_fd_filestat_set_times" (func $__wasi_fd_filestat_set_times))
  (export "__wasi_fd_pread" (func $__wasi_fd_pread))
  (export "__wasi_fd_prestat_get" (func $__wasi_fd_prestat_get))
  (export "__wasi_fd_prestat_dir_name" (func $__wasi_fd_prestat_dir_name))
  (export "__wasi_fd_pwrite" (func $__wasi_fd_pwrite))
  (export "__wasi_fd_read" (func $__wasi_fd_read))
  (export "__wasi_fd_readdir" (func $__wasi_fd_readdir))
  (export "__wasi_fd_renumber" (func $__wasi_fd_renumber))
  (export "__wasi_fd_seek" (func $__wasi_fd_seek))
  (export "__wasi_fd_sync" (func $__wasi_fd_sync))
  (export "__wasi_fd_tell" (func $__wasi_fd_tell))
  (export "__wasi_fd_write" (func $__wasi_fd_write))
  (export "__wasi_path_create_directory" (func $__wasi_path_create_directory))
  (export "__wasi_path_filestat_get" (func $__wasi_path_filestat_get))
  (export "__wasi_path_filestat_set_times" (func $__wasi_path_filestat_set_times))
  (export "__wasi_path_link" (func $__wasi_path_link))
  (export "__wasi_path_open" (func $__wasi_path_open))
  (export "__wasi_path_readlink" (func $__wasi_path_readlink))
  (export "__wasi_path_remove_directory" (func $__wasi_path_remove_directory))
  (export "__wasi_path_rename" (func $__wasi_path_rename))
  (export "__wasi_path_symlink" (func $__wasi_path_symlink))
  (export "__wasi_path_unlink_file" (func $__wasi_path_unlink_file))
  (export "__wasi_poll_oneoff" (func $__wasi_poll_oneoff))
  (export "strlen" (func $strlen))
  (export "__wasi_sched_yield" (func $__wasi_sched_yield))
  (export "__wasi_random_get" (func $__wasi_random_get))
  (export "__wasi_sock_accept" (func $__wasi_sock_accept))
  (export "__wasi_sock_recv" (func $__wasi_sock_recv))
  (export "__wasi_sock_send" (func $__wasi_sock_send))
  (export "__wasi_sock_shutdown" (func $__wasi_sock_shutdown))
  (export "__funcs_on_exit" (func $dummy))
  (export "__stdio_exit" (func $dummy))
  (export "exit" (func $exit))
  (export "__dso_handle" (global 5))
  (export "__data_end" (global 6))
  (export "__stack_low" (global 7))
  (export "__stack_high" (global 8))
  (export "__global_base" (global 9))
  (export "__heap_base" (global 10))
  (export "__heap_end" (global 11))
  (export "__table_base" (global 12))
  (data $.data (i32.const 1024) "\01\00\00\00"))
