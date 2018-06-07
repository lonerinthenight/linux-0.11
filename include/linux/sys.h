extern int sys_setup();
extern int sys_exit();
extern int sys_fork();
extern int sys_read();
extern int sys_write();
extern int sys_open();
extern int sys_close();
extern int sys_waitpid();
extern int sys_creat();
extern int sys_link();
extern int sys_unlink();
extern int sys_execve();
extern int sys_chdir();
extern int sys_time();
extern int sys_mknod();
extern int sys_chmod();
extern int sys_chown();
extern int sys_break();
extern int sys_stat();
extern int sys_lseek();
extern int sys_getpid();
extern int sys_mount();
extern int sys_umount();
extern int sys_setuid();
extern int sys_getuid();
extern int sys_stime();
extern int sys_ptrace();
extern int sys_alarm();
extern int sys_fstat();
extern int sys_pause();
extern int sys_utime();
extern int sys_stty();
extern int sys_gtty();
extern int sys_access();
extern int sys_nice();
extern int sys_ftime();
extern int sys_sync();
extern int sys_kill();
extern int sys_rename();
extern int sys_mkdir();
extern int sys_rmdir();
extern int sys_dup();
extern int sys_pipe();
extern int sys_times();
extern int sys_prof();
extern int sys_brk();
extern int sys_setgid();
extern int sys_getgid();
extern int sys_signal();
extern int sys_geteuid();
extern int sys_getegid();
extern int sys_acct();
extern int sys_phys();
extern int sys_lock();
extern int sys_ioctl();
extern int sys_fcntl();
extern int sys_mpx();
extern int sys_setpgid();
extern int sys_ulimit();
extern int sys_uname();
extern int sys_umask();
extern int sys_chroot();
extern int sys_ustat();
extern int sys_dup2();
extern int sys_getppid();
extern int sys_getpgrp();
extern int sys_setsid();
extern int sys_sigaction();
extern int sys_sgetmask();
extern int sys_ssetmask();
extern int sys_setreuid();
extern int sys_setregid();

fn_ptr sys_call_table[] = { 
/*00-04*/sys_setup, sys_exit, sys_fork, sys_read, sys_write, 
/*05-09*/sys_open, sys_close, sys_waitpid, sys_creat, sys_link,
/*10-14*/sys_unlink, sys_execve, sys_chdir, sys_time, sys_mknod, 
/*15-19*/sys_chmod,sys_chown, sys_break, sys_stat, sys_lseek, 
/*20-24*/sys_getpid, sys_mount,sys_umount, sys_setuid, sys_getuid, 
/*25-29*/sys_stime, sys_ptrace, sys_alarm,sys_fstat, sys_pause, 
/*30-34*/sys_utime, sys_stty, sys_gtty, sys_access,sys_nice, 
/*35-49*/sys_ftime, sys_sync, sys_kill, sys_rename, sys_mkdir,
/*40-44*/sys_rmdir, sys_dup, sys_pipe, sys_times, sys_prof, 
sys_brk, sys_setgid, sys_getgid, sys_signal, sys_geteuid, 
sys_getegid, sys_acct, sys_phys, sys_lock, sys_ioctl, 
sys_fcntl, sys_mpx, sys_setpgid, sys_ulimit, sys_uname, 
sys_umask, sys_chroot, sys_ustat, sys_dup2, sys_getppid,
sys_getpgrp, sys_setsid, sys_sigaction, sys_sgetmask, sys_ssetmask,
sys_setreuid,sys_setregid };