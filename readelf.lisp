(in-package :cl-user)

(defpackage :readelf
  (:use :cl :binary-parser))

(in-package :readelf)

;; reference: https://refspecs.linuxfoundation.org/elf/elf.pdf

(defun create-low-bit-list (bytes endianess)
  (let ((low-bits (loop for i upto (1- bytes) collect (* i 8))))
    (ecase endianess
      ((:big :msb) (setf low-bits (nreverse low-bits)))
      ((:little :lsb nil)))
    low-bits))

; TODO: implement endianess parsing
(define-binary-type unsigned-integer (bytes endianess)
  (:reader (in)
   (let ((low-bits (create-low-bit-list bytes endianess)))
     (loop with value = 0
	   for low-bit in low-bits
	   do (setf (ldb (byte 8 low-bit) value) (read-byte in))
	   finally (return value))))
  (:writer (out value)
   (let ((low-bits (create-low-bit-list bytes endianess)))
     (loop for low-bit in low-bits
	   do (write-byte (ldb (byte 8 low-bit) value) out)))))

(define-binary-type u8 ()  (unsigned-integer :bytes 1))
(define-binary-type u16 () (unsigned-integer :bytes 2))
(define-binary-type u32 () (unsigned-integer :bytes 4))
(define-binary-type u64 () (unsigned-integer :bytes 8))

(define-binary-type byte ()  (u8))
(define-binary-type word ()  (u16))
(define-binary-type dword () (u32))
(define-binary-type qword () (u64))

(define-binary-type signed-integer (bytes)
  (:reader (in)
   (let ((value (read-value 'unsigned-integer in 
			    :bytes bytes))
	 (bits (* 8 bytes)))
     (if (< (expt 2 (1- bits)) value)
	 value
	 (- (expt 2 bits) value))))
  (:writer (out value)
   (write-value 'unsigned-integer out (if (minusp value)
					  (+ (expt 2 (* 8 bytes)) value)
					  value)
		:bytes bytes)))

(define-binary-type s8 ()  (signed-integer :bytes 1))
(define-binary-type s16 () (signed-integer :bytes 2))
(define-binary-type s32 () (signed-integer :bytes 4))
(define-binary-type s64 () (signed-integer :bytes 8))

(define-binary-type elf32-addr ()  (u32))
(define-binary-type elf32-half ()  (u16))
(define-binary-type elf32-off ()   (u32))
(define-binary-type elf32-sword () (s32))
(define-binary-type elf32-word ()  (u32))

(define-binary-type uchar ()
  (:reader (in)
   (let ((code (read-byte in)))
     (code-char code)))
  (:writer (out char)
   (let ((code (char-code char)))
     (if (<= 0 code #xff)
	 (write-byte code out)
	 (error "Illegal unsigned char: character ~c with code: ~d" char code)))))

;; (define-binary-type uchar-string (length))

(define-binary-literal elfmag0 (uchar) (code-char #x7f))
(define-binary-literal elfmag1 (uchar) #\E)
(define-binary-literal elfmag2 (uchar) #\L)
(define-binary-literal elfmag3 (uchar) #\F)

(define-binary-enum ei-class (u8)
  (elfclassname elfclass32 elfclass64))

(define-binary-enum ei-data (u8)
  (elfdatanone elfdata2lsb elfdata2msb))

(define-binary-enum ei-version (u8)
  (ev-none ev-current))

(define-binary-class e-ident ()
  ((ei-mag0 elfmag0)
   (ei-mag1 elfmag1)
   (ei-mag2 elfmag2)
   (ei-mag3 elfmag3)
   (ei-class ei-class)
   (ei-data ei-data)
   (ei-version ei-version)
   (ei-pad (unsigned-integer :bytes 9))))

(define-binary-enum e-type (elf32-half)
  (et-none et-rel et-exec et-dyn et-core
   (et-loproc #xff00)
   (et-hiproc #xffff)))

(define-binary-enum e-machine (elf32-half)
  (et-none em-m32 em-sparc em-386 em-68k em-88k em-860 em-mips em-mips-rs4-be
   reserved11 reserved12 reserved13 reserved14 reserved15 reserved16
   (em-amd-x86-64 #x3e))) ; TODO: add other machines
  

(define-binary-enum e-version (elf32-word)
  (ev-none ev-current))

(define-binary-class elf-header ()
  ((e-ident      e-ident)
   (e-type	 e-type)
   (e-machine	 e-machine)
   (e-version	 e-version)
   (e-entry	 elf32-addr)
   (e-phoff	 elf32-off)
   (e-shoff	 elf32-off)
   (e-flags	 elf32-word)
   (e-ehsize	 elf32-half)
   (e-phentsize	 elf32-half)
   (e-phnum	 elf32-half)
   (e-shentsize	 elf32-half)
   (e-shnum	 elf32-half)
   (e-shstrndx	 elf32-half)))
