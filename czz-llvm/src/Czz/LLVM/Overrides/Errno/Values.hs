-- |
-- Module           : Czz.LLVM.Overrides.Errno.Values
-- Description      : @errno@
-- Copyright        : (c) Brian Langston Barrett, 2022
-- License          : BSD3
-- Maintainer       : Langston Barrett <langston.barrett@gmail.com>
-- Stability        : provisional
--
-- See @errno -l@

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}


module Czz.LLVM.Overrides.Errno.Values
  ( Errno
  , errnoToInt
  , errnoToBV
  , errnoToSymBV
  -- * Values
  , eperm
  , enoent
  , esrch
  , eintr
  , eio
  , enxio
  , e2big
  , enoexec
  , ebadf
  , echild
  , eagain
  , enomem
  , eacces
  , efault
  , enotblk
  , ebusy
  , eexist
  , exdev
  , enodev
  , enotdir
  , eisdir
  , einval
  , enfile
  , emfile
  , enotty
  , etxtbsy
  , efbig
  , enospc
  , espipe
  , erofs
  , emlink
  , epipe
  , edom
  , erange
  , edeadlk
  , enametoolong
  , enolck
  , enosys
  , enotempty
  , eloop
  , ewouldblock
  , enomsg
  , eidrm
  , echrng
  , el2nsync
  , el3hlt
  , el3rst
  , elnrng
  , eunatch
  , enocsi
  , el2hlt
  , ebade
  , ebadr
  , exfull
  , enoano
  , ebadrqc
  , ebadslt
  , edeadlock
  , ebfont
  , enostr
  , enodata
  , etime
  , enosr
  , enonet
  , enopkg
  , eremote
  , enolink
  , eadv
  , esrmnt
  , ecomm
  , eproto
  , emultihop
  , edotdot
  , ebadmsg
  , eoverflow
  , enotuniq
  , ebadfd
  , eremchg
  , elibacc
  , elibbad
  , elibscn
  , elibmax
  , elibexec
  , eilseq
  , erestart
  , estrpipe
  , eusers
  , enotsock
  , edestaddrreq
  , emsgsize
  , eprototype
  , enoprotoopt
  , eprotonosupport
  , esocktnosupport
  , eopnotsupp
  , epfnosupport
  , eafnosupport
  , eaddrinuse
  , eaddrnotavail
  , enetdown
  , enetunreach
  , enetreset
  , econnaborted
  , econnreset
  , enobufs
  , eisconn
  , enotconn
  , eshutdown
  , etoomanyrefs
  , etimedout
  , econnrefused
  , ehostdown
  , ehostunreach
  , ealready
  , einprogress
  , estale
  , euclean
  , enotnam
  , enavail
  , eisnam
  , eremoteio
  , edquot
  , enomedium
  , emediumtype
  , ecanceled
  , enokey
  , ekeyexpired
  , ekeyrevoked
  , ekeyrejected
  , eownerdead
  , enotrecoverable
  , erfkill
  , ehwpoison
  , enotsup
  )
where

import           Data.BitVector.Sized (BV)
import qualified Data.BitVector.Sized as BV

import           What4.Interface (IsSymExprBuilder, SymBV)
import qualified What4.Interface as What4

newtype Errno = Errno { getErrno :: Int }

errnoToInt :: Errno -> Int
errnoToInt = getErrno

errnoToBV :: Errno -> BV 32
errnoToBV = BV.mkBV (BV.knownNat @32) . toInteger . errnoToInt

errnoToSymBV :: IsSymExprBuilder sym => sym -> Errno -> IO (SymBV sym 32)
errnoToSymBV sym = What4.bvLit sym (BV.knownNat @32) . errnoToBV

--------------------------------------------------------------------------------
-- Values

-- | "Operation not permitted"
eperm :: Errno
eperm = Errno 1

-- | "No such file or directory"
enoent :: Errno
enoent = Errno 2

-- | "No such process"
esrch :: Errno
esrch = Errno 3

-- | "Interrupted system call"
eintr :: Errno
eintr = Errno 4

-- | "Input/output error"
eio :: Errno
eio = Errno 5

-- | "No such device or address"
enxio :: Errno
enxio = Errno 6

-- | "Argument list too long"
e2big :: Errno
e2big = Errno 7

-- | "Exec format error"
enoexec :: Errno
enoexec = Errno 8

-- | "Bad file descriptor"
ebadf :: Errno
ebadf = Errno 9

-- | "No child processes"
echild :: Errno
echild = Errno 10

-- | "Resource temporarily unavailable"
eagain :: Errno
eagain = Errno 11

-- | "Cannot allocate memory"
enomem :: Errno
enomem = Errno 12

-- | "Permission denied"
eacces :: Errno
eacces = Errno 13

-- | "Bad address"
efault :: Errno
efault = Errno 14

-- | "Block device required"
enotblk :: Errno
enotblk = Errno 15

-- | "Device or resource busy"
ebusy :: Errno
ebusy = Errno 16

-- | "File exists"
eexist :: Errno
eexist = Errno 17

-- | "Invalid cross-device link"
exdev :: Errno
exdev = Errno 18

-- | "No such device"
enodev :: Errno
enodev = Errno 19

-- | "Not a directory"
enotdir :: Errno
enotdir = Errno 20

-- | "Is a directory"
eisdir :: Errno
eisdir = Errno 21

-- | "Invalid argument"
einval :: Errno
einval = Errno 22

-- | "Too many open files in system"
enfile :: Errno
enfile = Errno 23

-- | "Too many open files"
emfile :: Errno
emfile = Errno 24

-- | "Inappropriate ioctl for device"
enotty :: Errno
enotty = Errno 25

-- | "Text file busy"
etxtbsy :: Errno
etxtbsy = Errno 26

-- | "File too large"
efbig :: Errno
efbig = Errno 27

-- | "No space left on device"
enospc :: Errno
enospc = Errno 28

-- | "Illegal seek"
espipe :: Errno
espipe = Errno 29

-- | "Read-only file system"
erofs :: Errno
erofs = Errno 30

-- | "Too many links"
emlink :: Errno
emlink = Errno 31

-- | "Broken pipe"
epipe :: Errno
epipe = Errno 32

-- | "Numerical argument out of domain"
edom :: Errno
edom = Errno 33

-- | "Numerical result out of range"
erange :: Errno
erange = Errno 34

-- | "Resource deadlock avoided"
edeadlk :: Errno
edeadlk = Errno 35

-- | "File name too long"
enametoolong :: Errno
enametoolong = Errno 36

-- | "No locks available"
enolck :: Errno
enolck = Errno 37

-- | "Function not implemented"
enosys :: Errno
enosys = Errno 38

-- | "Directory not empty"
enotempty :: Errno
enotempty = Errno 39

-- | "Too many levels of symbolic links"
eloop :: Errno
eloop = Errno 40

-- | "Resource temporarily unavailable"
ewouldblock :: Errno
ewouldblock = Errno 11

-- | "No message of desired type"
enomsg :: Errno
enomsg = Errno 42

-- | "Identifier removed"
eidrm :: Errno
eidrm = Errno 43

-- | "Channel number out of range"
echrng :: Errno
echrng = Errno 44

-- | "Level 2 not synchronized"
el2nsync :: Errno
el2nsync = Errno 45

-- | "Level 3 halted"
el3hlt :: Errno
el3hlt = Errno 46

-- | "Level 3 reset"
el3rst :: Errno
el3rst = Errno 47

-- | "Link number out of range"
elnrng :: Errno
elnrng = Errno 48

-- | "Protocol driver not attached"
eunatch :: Errno
eunatch = Errno 49

-- | "No CSI structure available"
enocsi :: Errno
enocsi = Errno 50

-- | "Level 2 halted"
el2hlt :: Errno
el2hlt = Errno 51

-- | "Invalid exchange"
ebade :: Errno
ebade = Errno 52

-- | "Invalid request descriptor"
ebadr :: Errno
ebadr = Errno 53

-- | "Exchange full"
exfull :: Errno
exfull = Errno 54

-- | "No anode"
enoano :: Errno
enoano = Errno 55

-- | "Invalid request code"
ebadrqc :: Errno
ebadrqc = Errno 56

-- | "Invalid slot"
ebadslt :: Errno
ebadslt = Errno 57

-- | "Resource deadlock avoided"
edeadlock :: Errno
edeadlock = Errno 35

-- | "Bad font file format"
ebfont :: Errno
ebfont = Errno 59

-- | "Device not a stream"
enostr :: Errno
enostr = Errno 60

-- | "No data available"
enodata :: Errno
enodata = Errno 61

-- | "Timer expired"
etime :: Errno
etime = Errno 62

-- | "Out of streams resources"
enosr :: Errno
enosr = Errno 63

-- | "Machine is not on the network"
enonet :: Errno
enonet = Errno 64

-- | "Package not installed"
enopkg :: Errno
enopkg = Errno 65

-- | "Object is remote"
eremote :: Errno
eremote = Errno 66

-- | "Link has been severed"
enolink :: Errno
enolink = Errno 67

-- | "Advertise error"
eadv :: Errno
eadv = Errno 68

-- | "Srmount error"
esrmnt :: Errno
esrmnt = Errno 69

-- | "Communication error on send"
ecomm :: Errno
ecomm = Errno 70

-- | "Protocol error"
eproto :: Errno
eproto = Errno 71

-- | "Multihop attempted"
emultihop :: Errno
emultihop = Errno 72

-- | "RFS specific error"
edotdot :: Errno
edotdot = Errno 73

-- | "Bad message"
ebadmsg :: Errno
ebadmsg = Errno 74

-- | "Value too large for defined data type"
eoverflow :: Errno
eoverflow = Errno 75

-- | "Name not unique on network"
enotuniq :: Errno
enotuniq = Errno 76

-- | "File descriptor in bad state"
ebadfd :: Errno
ebadfd = Errno 77

-- | "Remote address changed"
eremchg :: Errno
eremchg = Errno 78

-- | "Can not access a needed shared library"
elibacc :: Errno
elibacc = Errno 79

-- | "Accessing a corrupted shared library"
elibbad :: Errno
elibbad = Errno 80

-- | ".lib section in a.out corrupted"
elibscn :: Errno
elibscn = Errno 81

-- | "Attempting to link in too many shared libraries"
elibmax :: Errno
elibmax = Errno 82

-- | "Cannot exec a shared library directly"
elibexec :: Errno
elibexec = Errno 83

-- | "Invalid or incomplete multibyte or wide character"
eilseq :: Errno
eilseq = Errno 84

-- | "Interrupted system call should be restarted"
erestart :: Errno
erestart = Errno 85

-- | "Streams pipe error"
estrpipe :: Errno
estrpipe = Errno 86

-- | "Too many users"
eusers :: Errno
eusers = Errno 87

-- | "Socket operation on non-socket"
enotsock :: Errno
enotsock = Errno 88

-- | "Destination address required"
edestaddrreq :: Errno
edestaddrreq = Errno 89

-- | "Message too long"
emsgsize :: Errno
emsgsize = Errno 90

-- | "Protocol wrong type for socket"
eprototype :: Errno
eprototype = Errno 91

-- | "Protocol not available"
enoprotoopt :: Errno
enoprotoopt = Errno 92

-- | "Protocol not supported"
eprotonosupport :: Errno
eprotonosupport = Errno 93

-- | "Socket type not supported"
esocktnosupport :: Errno
esocktnosupport = Errno 94

-- | "Operation not supported"
eopnotsupp :: Errno
eopnotsupp = Errno 95

-- | "Protocol family not supported"
epfnosupport :: Errno
epfnosupport = Errno 96

-- | "Address family not supported by protocol"
eafnosupport :: Errno
eafnosupport = Errno 97

-- | "Address already in use"
eaddrinuse :: Errno
eaddrinuse = Errno 98

-- | "Cannot assign requested address"
eaddrnotavail :: Errno
eaddrnotavail = Errno 99

-- | "Network is down"
enetdown :: Errno
enetdown = Errno 100

-- | "Network is unreachable"
enetunreach :: Errno
enetunreach = Errno 101

-- | "Network dropped connection on reset"
enetreset :: Errno
enetreset = Errno 102

-- | "Software caused connection abort"
econnaborted :: Errno
econnaborted = Errno 103

-- | "Connection reset by peer"
econnreset :: Errno
econnreset = Errno 104

-- | "No buffer space available"
enobufs :: Errno
enobufs = Errno 105

-- | "Transport endpoint is already connected"
eisconn :: Errno
eisconn = Errno 106

-- | "Transport endpoint is not connected"
enotconn :: Errno
enotconn = Errno 107

-- | "Cannot send after transport endpoint shutdown"
eshutdown :: Errno
eshutdown = Errno 108

-- | "Too many references: cannot splice"
etoomanyrefs :: Errno
etoomanyrefs = Errno 109

-- | "Connection timed out"
etimedout :: Errno
etimedout = Errno 110

-- | "Connection refused"
econnrefused :: Errno
econnrefused = Errno 111

-- | "Host is down"
ehostdown :: Errno
ehostdown = Errno 112

-- | "No route to host"
ehostunreach :: Errno
ehostunreach = Errno 113

-- | "Operation already in progress"
ealready :: Errno
ealready = Errno 114

-- | "Operation now in progress"
einprogress :: Errno
einprogress = Errno 115

-- | "Stale file handle"
estale :: Errno
estale = Errno 116

-- | "Structure needs cleaning"
euclean :: Errno
euclean = Errno 117

-- | "Not a XENIX named type file"
enotnam :: Errno
enotnam = Errno 118

-- | "No XENIX semaphores available"
enavail :: Errno
enavail = Errno 119

-- | "Is a named type file"
eisnam :: Errno
eisnam = Errno 120

-- | "Remote I/O error"
eremoteio :: Errno
eremoteio = Errno 121

-- | "Disk quota exceeded"
edquot :: Errno
edquot = Errno 122

-- | "No medium found"
enomedium :: Errno
enomedium = Errno 123

-- | "Wrong medium type"
emediumtype :: Errno
emediumtype = Errno 124

-- | "Operation canceled"
ecanceled :: Errno
ecanceled = Errno 125

-- | "Required key not available"
enokey :: Errno
enokey = Errno 126

-- | "Key has expired"
ekeyexpired :: Errno
ekeyexpired = Errno 127

-- | "Key has been revoked"
ekeyrevoked :: Errno
ekeyrevoked = Errno 128

-- | "Key was rejected by service"
ekeyrejected :: Errno
ekeyrejected = Errno 129

-- | "Owner died"
eownerdead :: Errno
eownerdead = Errno 130

-- | "State not recoverable"
enotrecoverable :: Errno
enotrecoverable = Errno 131

-- | "Operation not possible due to RF-kill"
erfkill :: Errno
erfkill = Errno 132

-- | "Memory page has hardware error"
ehwpoison :: Errno
ehwpoison = Errno 133

-- | "Operation not supported"
enotsup :: Errno
enotsup = Errno 95
