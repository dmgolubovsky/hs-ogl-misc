------------------------------------------------------------------
-- |
-- Module      :  System.IO9.Error
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
--
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
--
--
--
-- Plan9-style errors definition: based on /sys/src/9/port/error.h
------------------------------------------------------------------

-- File generated automatically - do not edit.

module System.IO9.Error (
  NineError (..)) where

import Data.Typeable
import Control.Exception

data NineError = OtherError String
 | Enoerror	-- ^ no error
 | Emount	-- ^ inconsistent mount
 | Eunmount	-- ^ not mounted
 | Eismtpt	-- ^ is a mount point
 | Eunion	-- ^ not in union
 | Emountrpc	-- ^ mount rpc error
 | Eshutdown	-- ^ device shut down
 | Enocreate	-- ^ mounted directory forbids creation
 | Enonexist	-- ^ file does not exist
 | Eexist	-- ^ file already exists
 | Ebadsharp	-- ^ unknown device in # filename
 | Enotdir	-- ^ not a directory
 | Eisdir	-- ^ file is a directory
 | Ebadchar	-- ^ bad character in file name
 | Efilename	-- ^ file name syntax
 | Eperm	-- ^ permission denied
 | Ebadusefd	-- ^ inappropriate use of fd
 | Ebadarg	-- ^ bad arg in system call
 | Einuse	-- ^ device or object already in use
 | Eio	-- ^ i/o error
 | Etoobig	-- ^ read or write too large
 | Etoosmall	-- ^ read or write too small
 | Enoport	-- ^ network port not available
 | Ehungup	-- ^ i/o on hungup channel
 | Ebadctl	-- ^ bad process or channel control request
 | Enodev	-- ^ no free devices
 | Eprocdied	-- ^ process exited
 | Enochild	-- ^ no living children
 | Eioload	-- ^ i/o error in demand load
 | Enovmem	-- ^ virtual memory allocation failed
 | Ebadfd	-- ^ fd out of range or not open
 | Enofd	-- ^ no free file descriptors
 | Eisstream	-- ^ seek on a stream
 | Ebadexec	-- ^ exec header invalid
 | Etimedout	-- ^ connection timed out
 | Econrefused	-- ^ connection refused
 | Econinuse	-- ^ connection in use
 | Eintr	-- ^ interrupted
 | Enomem	-- ^ kernel allocate failed
 | Enoswap	-- ^ swap space full
 | Esoverlap	-- ^ segments overlap
 | Emouseset	-- ^ mouse type already set
 | Eshort	-- ^ i/o count too small
 | Egreg	-- ^ jmk added reentrancy for threads
 | Ebadspec	-- ^ bad attach specifier
 | Enoreg	-- ^ process has no saved registers
 | Enoattach	-- ^ mount/attach disallowed
 | Eshortstat	-- ^ stat buffer too small
 | Ebadstat	-- ^ malformed stat buffer
 | Enegoff	-- ^ negative i/o offset
 | Ecmdargs	-- ^ wrong #args in control message
 | Ebadip	-- ^ bad ip address syntax
 | Edirseek	-- ^ seek in directory
    deriving (Eq, Typeable)

instance Show NineError where
  show (OtherError s) = s
  show Enoerror = "no error"
  show Emount = "inconsistent mount"
  show Eunmount = "not mounted"
  show Eismtpt = "is a mount point"
  show Eunion = "not in union"
  show Emountrpc = "mount rpc error"
  show Eshutdown = "device shut down"
  show Enocreate = "mounted directory forbids creation"
  show Enonexist = "file does not exist"
  show Eexist = "file already exists"
  show Ebadsharp = "unknown device in # filename"
  show Enotdir = "not a directory"
  show Eisdir = "file is a directory"
  show Ebadchar = "bad character in file name"
  show Efilename = "file name syntax"
  show Eperm = "permission denied"
  show Ebadusefd = "inappropriate use of fd"
  show Ebadarg = "bad arg in system call"
  show Einuse = "device or object already in use"
  show Eio = "i/o error"
  show Etoobig = "read or write too large"
  show Etoosmall = "read or write too small"
  show Enoport = "network port not available"
  show Ehungup = "i/o on hungup channel"
  show Ebadctl = "bad process or channel control request"
  show Enodev = "no free devices"
  show Eprocdied = "process exited"
  show Enochild = "no living children"
  show Eioload = "i/o error in demand load"
  show Enovmem = "virtual memory allocation failed"
  show Ebadfd = "fd out of range or not open"
  show Enofd = "no free file descriptors"
  show Eisstream = "seek on a stream"
  show Ebadexec = "exec header invalid"
  show Etimedout = "connection timed out"
  show Econrefused = "connection refused"
  show Econinuse = "connection in use"
  show Eintr = "interrupted"
  show Enomem = "kernel allocate failed"
  show Enoswap = "swap space full"
  show Esoverlap = "segments overlap"
  show Emouseset = "mouse type already set"
  show Eshort = "i/o count too small"
  show Egreg = "jmk added reentrancy for threads"
  show Ebadspec = "bad attach specifier"
  show Enoreg = "process has no saved registers"
  show Enoattach = "mount/attach disallowed"
  show Eshortstat = "stat buffer too small"
  show Ebadstat = "malformed stat buffer"
  show Enegoff = "negative i/o offset"
  show Ecmdargs = "wrong #args in control message"
  show Ebadip = "bad ip address syntax"
  show Edirseek = "seek in directory"

instance Exception NineError

