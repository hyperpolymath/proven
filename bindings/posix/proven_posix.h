/*
 * SPDX-License-Identifier: Apache-2.0
 * Proven - Formally verified safety primitives for POSIX
 *
 * Safe wrappers for POSIX system calls and utilities.
 * Provides bounds checking, resource management, and explicit
 * error handling for Unix system programming.
 *
 * Compatible with: Linux, macOS, FreeBSD, OpenBSD, NetBSD, Solaris
 *
 * Usage:
 *   #include "proven_posix.h"
 *
 *   proven_fd_t fd;
 *   if (proven_open(&fd, "/path/to/file", O_RDONLY, 0) == PROVEN_OK) {
 *       // Use fd.fd safely
 *       proven_close(&fd);
 *   }
 */

#ifndef PROVEN_POSIX_H
#define PROVEN_POSIX_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <poll.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================== */
/* RESULT TYPE - Explicit error handling                                      */
/* ========================================================================== */

typedef enum {
    PROVEN_OK = 0,
    PROVEN_ERR_INVALID_ARG = 1,
    PROVEN_ERR_SYSCALL = 2,
    PROVEN_ERR_BOUNDS = 3,
    PROVEN_ERR_NULL = 4,
    PROVEN_ERR_OVERFLOW = 5,
    PROVEN_ERR_RESOURCE = 6,
    PROVEN_ERR_PERMISSION = 7,
    PROVEN_ERR_NOT_FOUND = 8,
    PROVEN_ERR_TIMEOUT = 9,
    PROVEN_ERR_CLOSED = 10
} proven_result_t;

/* Get human-readable error message */
static inline const char* proven_strerror(proven_result_t result) {
    switch (result) {
        case PROVEN_OK: return "Success";
        case PROVEN_ERR_INVALID_ARG: return "Invalid argument";
        case PROVEN_ERR_SYSCALL: return "System call failed";
        case PROVEN_ERR_BOUNDS: return "Out of bounds";
        case PROVEN_ERR_NULL: return "Null pointer";
        case PROVEN_ERR_OVERFLOW: return "Overflow";
        case PROVEN_ERR_RESOURCE: return "Resource exhausted";
        case PROVEN_ERR_PERMISSION: return "Permission denied";
        case PROVEN_ERR_NOT_FOUND: return "Not found";
        case PROVEN_ERR_TIMEOUT: return "Timeout";
        case PROVEN_ERR_CLOSED: return "Resource closed";
        default: return "Unknown error";
    }
}

/* ========================================================================== */
/* SAFE FILE DESCRIPTOR                                                       */
/* ========================================================================== */

/* File descriptor with validity tracking */
typedef struct {
    int fd;
    bool valid;
    int open_flags;
} proven_fd_t;

/* Initialize invalid fd */
static inline void proven_fd_init(proven_fd_t* pfd) {
    if (pfd) {
        pfd->fd = -1;
        pfd->valid = false;
        pfd->open_flags = 0;
    }
}

/* Check if fd is valid */
static inline bool proven_fd_is_valid(const proven_fd_t* pfd) {
    return pfd && pfd->valid && pfd->fd >= 0;
}

/* Safe open with validation */
static inline proven_result_t proven_open(proven_fd_t* pfd, const char* path,
                                          int flags, mode_t mode) {
    if (!pfd) return PROVEN_ERR_NULL;
    if (!path) return PROVEN_ERR_NULL;

    proven_fd_init(pfd);

    int fd = open(path, flags, mode);
    if (fd < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    pfd->fd = fd;
    pfd->valid = true;
    pfd->open_flags = flags;
    return PROVEN_OK;
}

/* Safe close with invalidation */
static inline proven_result_t proven_close(proven_fd_t* pfd) {
    if (!pfd) return PROVEN_ERR_NULL;
    if (!proven_fd_is_valid(pfd)) return PROVEN_ERR_CLOSED;

    int result = close(pfd->fd);
    pfd->fd = -1;
    pfd->valid = false;
    pfd->open_flags = 0;

    return (result == 0) ? PROVEN_OK : PROVEN_ERR_SYSCALL;
}

/* Safe read with bounds checking */
static inline proven_result_t proven_read(const proven_fd_t* pfd, void* buf,
                                          size_t count, ssize_t* bytes_read) {
    if (!pfd) return PROVEN_ERR_NULL;
    if (!buf && count > 0) return PROVEN_ERR_NULL;
    if (!proven_fd_is_valid(pfd)) return PROVEN_ERR_CLOSED;
    if (!bytes_read) return PROVEN_ERR_NULL;

    ssize_t result = read(pfd->fd, buf, count);
    if (result < 0) {
        *bytes_read = 0;
        return PROVEN_ERR_SYSCALL;
    }

    *bytes_read = result;
    return PROVEN_OK;
}

/* Safe write with bounds checking */
static inline proven_result_t proven_write(const proven_fd_t* pfd, const void* buf,
                                           size_t count, ssize_t* bytes_written) {
    if (!pfd) return PROVEN_ERR_NULL;
    if (!buf && count > 0) return PROVEN_ERR_NULL;
    if (!proven_fd_is_valid(pfd)) return PROVEN_ERR_CLOSED;
    if (!bytes_written) return PROVEN_ERR_NULL;

    ssize_t result = write(pfd->fd, buf, count);
    if (result < 0) {
        *bytes_written = 0;
        return PROVEN_ERR_SYSCALL;
    }

    *bytes_written = result;
    return PROVEN_OK;
}

/* Safe lseek */
static inline proven_result_t proven_lseek(const proven_fd_t* pfd, off_t offset,
                                           int whence, off_t* new_offset) {
    if (!pfd) return PROVEN_ERR_NULL;
    if (!proven_fd_is_valid(pfd)) return PROVEN_ERR_CLOSED;
    if (!new_offset) return PROVEN_ERR_NULL;
    if (whence != SEEK_SET && whence != SEEK_CUR && whence != SEEK_END) {
        return PROVEN_ERR_INVALID_ARG;
    }

    off_t result = lseek(pfd->fd, offset, whence);
    if (result == (off_t)-1) {
        *new_offset = 0;
        return PROVEN_ERR_SYSCALL;
    }

    *new_offset = result;
    return PROVEN_OK;
}

/* ========================================================================== */
/* SAFE SOCKET                                                                */
/* ========================================================================== */

typedef struct {
    int sockfd;
    bool valid;
    int domain;
    int type;
    int protocol;
    bool connected;
    bool listening;
} proven_socket_t;

static inline void proven_socket_init(proven_socket_t* sock) {
    if (sock) {
        sock->sockfd = -1;
        sock->valid = false;
        sock->domain = 0;
        sock->type = 0;
        sock->protocol = 0;
        sock->connected = false;
        sock->listening = false;
    }
}

static inline bool proven_socket_is_valid(const proven_socket_t* sock) {
    return sock && sock->valid && sock->sockfd >= 0;
}

/* Safe socket creation */
static inline proven_result_t proven_socket_create(proven_socket_t* sock,
                                                   int domain, int type, int protocol) {
    if (!sock) return PROVEN_ERR_NULL;

    proven_socket_init(sock);

    int fd = socket(domain, type, protocol);
    if (fd < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    sock->sockfd = fd;
    sock->valid = true;
    sock->domain = domain;
    sock->type = type;
    sock->protocol = protocol;
    return PROVEN_OK;
}

/* Safe socket close */
static inline proven_result_t proven_socket_close(proven_socket_t* sock) {
    if (!sock) return PROVEN_ERR_NULL;
    if (!proven_socket_is_valid(sock)) return PROVEN_ERR_CLOSED;

    int result = close(sock->sockfd);
    proven_socket_init(sock);

    return (result == 0) ? PROVEN_OK : PROVEN_ERR_SYSCALL;
}

/* Safe bind with port validation */
static inline proven_result_t proven_socket_bind(proven_socket_t* sock,
                                                 const struct sockaddr* addr,
                                                 socklen_t addrlen) {
    if (!sock) return PROVEN_ERR_NULL;
    if (!addr) return PROVEN_ERR_NULL;
    if (!proven_socket_is_valid(sock)) return PROVEN_ERR_CLOSED;

    /* Validate port for IPv4/IPv6 */
    if (addr->sa_family == AF_INET) {
        const struct sockaddr_in* addr4 = (const struct sockaddr_in*)addr;
        uint16_t port = ntohs(addr4->sin_port);
        if (port == 0) {
            /* Port 0 means "assign any available port" - this is valid */
        }
    } else if (addr->sa_family == AF_INET6) {
        const struct sockaddr_in6* addr6 = (const struct sockaddr_in6*)addr;
        uint16_t port = ntohs(addr6->sin6_port);
        if (port == 0) {
            /* Port 0 means "assign any available port" - this is valid */
        }
    }

    if (bind(sock->sockfd, addr, addrlen) < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    return PROVEN_OK;
}

/* Safe listen */
static inline proven_result_t proven_socket_listen(proven_socket_t* sock, int backlog) {
    if (!sock) return PROVEN_ERR_NULL;
    if (!proven_socket_is_valid(sock)) return PROVEN_ERR_CLOSED;
    if (backlog < 0) return PROVEN_ERR_INVALID_ARG;

    /* Clamp backlog to reasonable value */
    if (backlog > SOMAXCONN) {
        backlog = SOMAXCONN;
    }

    if (listen(sock->sockfd, backlog) < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    sock->listening = true;
    return PROVEN_OK;
}

/* Safe accept */
static inline proven_result_t proven_socket_accept(proven_socket_t* listen_sock,
                                                   proven_socket_t* client_sock,
                                                   struct sockaddr* addr,
                                                   socklen_t* addrlen) {
    if (!listen_sock) return PROVEN_ERR_NULL;
    if (!client_sock) return PROVEN_ERR_NULL;
    if (!proven_socket_is_valid(listen_sock)) return PROVEN_ERR_CLOSED;
    if (!listen_sock->listening) return PROVEN_ERR_INVALID_ARG;

    proven_socket_init(client_sock);

    int fd = accept(listen_sock->sockfd, addr, addrlen);
    if (fd < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    client_sock->sockfd = fd;
    client_sock->valid = true;
    client_sock->domain = listen_sock->domain;
    client_sock->type = listen_sock->type;
    client_sock->protocol = listen_sock->protocol;
    client_sock->connected = true;
    return PROVEN_OK;
}

/* Safe connect */
static inline proven_result_t proven_socket_connect(proven_socket_t* sock,
                                                    const struct sockaddr* addr,
                                                    socklen_t addrlen) {
    if (!sock) return PROVEN_ERR_NULL;
    if (!addr) return PROVEN_ERR_NULL;
    if (!proven_socket_is_valid(sock)) return PROVEN_ERR_CLOSED;

    if (connect(sock->sockfd, addr, addrlen) < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    sock->connected = true;
    return PROVEN_OK;
}

/* Safe send */
static inline proven_result_t proven_socket_send(const proven_socket_t* sock,
                                                 const void* buf, size_t len,
                                                 int flags, ssize_t* sent) {
    if (!sock) return PROVEN_ERR_NULL;
    if (!buf && len > 0) return PROVEN_ERR_NULL;
    if (!proven_socket_is_valid(sock)) return PROVEN_ERR_CLOSED;
    if (!sent) return PROVEN_ERR_NULL;

    ssize_t result = send(sock->sockfd, buf, len, flags);
    if (result < 0) {
        *sent = 0;
        return PROVEN_ERR_SYSCALL;
    }

    *sent = result;
    return PROVEN_OK;
}

/* Safe recv */
static inline proven_result_t proven_socket_recv(const proven_socket_t* sock,
                                                 void* buf, size_t len,
                                                 int flags, ssize_t* received) {
    if (!sock) return PROVEN_ERR_NULL;
    if (!buf && len > 0) return PROVEN_ERR_NULL;
    if (!proven_socket_is_valid(sock)) return PROVEN_ERR_CLOSED;
    if (!received) return PROVEN_ERR_NULL;

    ssize_t result = recv(sock->sockfd, buf, len, flags);
    if (result < 0) {
        *received = 0;
        return PROVEN_ERR_SYSCALL;
    }

    *received = result;
    return PROVEN_OK;
}

/* ========================================================================== */
/* SAFE PROCESS MANAGEMENT                                                    */
/* ========================================================================== */

typedef struct {
    pid_t pid;
    bool valid;
    bool terminated;
    int exit_status;
} proven_process_t;

static inline void proven_process_init(proven_process_t* proc) {
    if (proc) {
        proc->pid = -1;
        proc->valid = false;
        proc->terminated = false;
        proc->exit_status = 0;
    }
}

/* Safe fork wrapper */
static inline proven_result_t proven_fork(proven_process_t* child) {
    if (!child) return PROVEN_ERR_NULL;

    proven_process_init(child);

    pid_t pid = fork();
    if (pid < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    if (pid == 0) {
        /* Child process */
        child->pid = getpid();
        child->valid = true;
    } else {
        /* Parent process */
        child->pid = pid;
        child->valid = true;
    }

    return PROVEN_OK;
}

/* Safe waitpid wrapper */
static inline proven_result_t proven_wait(proven_process_t* proc, int options) {
    if (!proc) return PROVEN_ERR_NULL;
    if (!proc->valid) return PROVEN_ERR_INVALID_ARG;
    if (proc->terminated) return PROVEN_ERR_INVALID_ARG;

    int status;
    pid_t result = waitpid(proc->pid, &status, options);

    if (result < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    if (result == 0 && (options & WNOHANG)) {
        /* Process still running */
        return PROVEN_OK;
    }

    proc->terminated = true;
    if (WIFEXITED(status)) {
        proc->exit_status = WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
        proc->exit_status = -WTERMSIG(status);
    }

    return PROVEN_OK;
}

/* ========================================================================== */
/* SAFE SIGNAL HANDLING                                                       */
/* ========================================================================== */

typedef struct {
    int signum;
    bool installed;
    struct sigaction old_action;
} proven_signal_handler_t;

static inline void proven_signal_init(proven_signal_handler_t* handler) {
    if (handler) {
        handler->signum = 0;
        handler->installed = false;
    }
}

/* Safe signal handler installation */
static inline proven_result_t proven_signal_install(proven_signal_handler_t* handler,
                                                    int signum,
                                                    void (*handler_func)(int)) {
    if (!handler) return PROVEN_ERR_NULL;
    if (!handler_func) return PROVEN_ERR_NULL;

    /* Validate signal number - don't allow SIGKILL or SIGSTOP */
    if (signum == SIGKILL || signum == SIGSTOP) {
        return PROVEN_ERR_INVALID_ARG;
    }
    if (signum < 1 || signum > 31) {
        return PROVEN_ERR_INVALID_ARG;
    }

    proven_signal_init(handler);

    struct sigaction sa;
    sa.sa_handler = handler_func;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;

    if (sigaction(signum, &sa, &handler->old_action) < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    handler->signum = signum;
    handler->installed = true;
    return PROVEN_OK;
}

/* Restore original signal handler */
static inline proven_result_t proven_signal_restore(proven_signal_handler_t* handler) {
    if (!handler) return PROVEN_ERR_NULL;
    if (!handler->installed) return PROVEN_ERR_INVALID_ARG;

    if (sigaction(handler->signum, &handler->old_action, NULL) < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    handler->installed = false;
    return PROVEN_OK;
}

/* ========================================================================== */
/* SAFE MEMORY MAPPING                                                        */
/* ========================================================================== */

typedef struct {
    void* addr;
    size_t length;
    bool valid;
    int prot;
    int flags;
} proven_mmap_t;

static inline void proven_mmap_init(proven_mmap_t* mapping) {
    if (mapping) {
        mapping->addr = NULL;
        mapping->length = 0;
        mapping->valid = false;
        mapping->prot = 0;
        mapping->flags = 0;
    }
}

/* Safe mmap wrapper */
static inline proven_result_t proven_mmap(proven_mmap_t* mapping,
                                          void* addr, size_t length,
                                          int prot, int flags,
                                          int fd, off_t offset) {
    if (!mapping) return PROVEN_ERR_NULL;
    if (length == 0) return PROVEN_ERR_INVALID_ARG;

    proven_mmap_init(mapping);

    void* result = mmap(addr, length, prot, flags, fd, offset);
    if (result == MAP_FAILED) {
        return PROVEN_ERR_SYSCALL;
    }

    mapping->addr = result;
    mapping->length = length;
    mapping->valid = true;
    mapping->prot = prot;
    mapping->flags = flags;
    return PROVEN_OK;
}

/* Safe munmap wrapper */
static inline proven_result_t proven_munmap(proven_mmap_t* mapping) {
    if (!mapping) return PROVEN_ERR_NULL;
    if (!mapping->valid) return PROVEN_ERR_INVALID_ARG;

    if (munmap(mapping->addr, mapping->length) < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    proven_mmap_init(mapping);
    return PROVEN_OK;
}

/* ========================================================================== */
/* SAFE POLL/SELECT                                                           */
/* ========================================================================== */

/* Safe poll wrapper with timeout bounds */
static inline proven_result_t proven_poll(struct pollfd* fds, nfds_t nfds,
                                          int timeout_ms, int* ready_count) {
    if (!fds && nfds > 0) return PROVEN_ERR_NULL;
    if (!ready_count) return PROVEN_ERR_NULL;

    /* Clamp timeout to reasonable value (max 1 hour) */
    if (timeout_ms > 3600000) {
        timeout_ms = 3600000;
    }

    int result = poll(fds, nfds, timeout_ms);
    if (result < 0) {
        *ready_count = 0;
        if (errno == EINTR) {
            return PROVEN_OK;  /* Interrupted, but not an error */
        }
        return PROVEN_ERR_SYSCALL;
    }

    *ready_count = result;
    return PROVEN_OK;
}

/* ========================================================================== */
/* SAFE PIPE                                                                  */
/* ========================================================================== */

typedef struct {
    proven_fd_t read_end;
    proven_fd_t write_end;
    bool valid;
} proven_pipe_t;

static inline void proven_pipe_init(proven_pipe_t* p) {
    if (p) {
        proven_fd_init(&p->read_end);
        proven_fd_init(&p->write_end);
        p->valid = false;
    }
}

/* Safe pipe creation */
static inline proven_result_t proven_pipe_create(proven_pipe_t* p) {
    if (!p) return PROVEN_ERR_NULL;

    proven_pipe_init(p);

    int pipefd[2];
    if (pipe(pipefd) < 0) {
        return PROVEN_ERR_SYSCALL;
    }

    p->read_end.fd = pipefd[0];
    p->read_end.valid = true;
    p->read_end.open_flags = O_RDONLY;

    p->write_end.fd = pipefd[1];
    p->write_end.valid = true;
    p->write_end.open_flags = O_WRONLY;

    p->valid = true;
    return PROVEN_OK;
}

/* Close both ends of pipe */
static inline proven_result_t proven_pipe_close(proven_pipe_t* p) {
    if (!p) return PROVEN_ERR_NULL;

    proven_result_t r1 = PROVEN_OK, r2 = PROVEN_OK;

    if (proven_fd_is_valid(&p->read_end)) {
        r1 = proven_close(&p->read_end);
    }
    if (proven_fd_is_valid(&p->write_end)) {
        r2 = proven_close(&p->write_end);
    }

    p->valid = false;

    return (r1 == PROVEN_OK && r2 == PROVEN_OK) ? PROVEN_OK : PROVEN_ERR_SYSCALL;
}

/* ========================================================================== */
/* UTILITY MACROS                                                             */
/* ========================================================================== */

/* Check and propagate errors */
#define PROVEN_TRY(expr) \
    do { \
        proven_result_t _result = (expr); \
        if (_result != PROVEN_OK) return _result; \
    } while (0)

/* Check and handle errors with cleanup */
#define PROVEN_TRY_CLEANUP(expr, cleanup) \
    do { \
        proven_result_t _result = (expr); \
        if (_result != PROVEN_OK) { cleanup; return _result; } \
    } while (0)

#ifdef __cplusplus
}
#endif

#endif /* PROVEN_POSIX_H */
