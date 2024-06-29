(in-package :cl-user)

(defpackage :readelf
  (:use :cl :binary-parser))

(in-package :readelf)

(defun generate-low-bit-list (bytes endianess)
  (loop for i upto (* 8 (1- bytes)) by 8
	collect i into low-bit-list
	finally (return (ecase endianess
			  ((:big :msb) (nreverse low-bit-list))
			  ((:little :lsb nil) low-bit-list)))))

(define-binary-type unsigned-integer (bytes endianess)
  (:reader (in)
   (loop with value = 0
	 for low-bit in (generate-low-bit-list bytes endianess)
	 do (setf (ldb (byte 8 low-bit) value) (read-byte in))
	 finally (return value)))
  (:writer (out value)
   (loop for low-bit in (generate-low-bit-list bytes endianess)
	 do (write-byte (ldb (byte 8 low-bit) value) out))))

(define-binary-type u8 ()  (unsigned-integer :bytes 1))
(define-binary-type u16 () (unsigned-integer :bytes 2))
(define-binary-type u32 () (unsigned-integer :bytes 4))
(define-binary-type u64 () (unsigned-integer :bytes 8))

(define-binary-type byte ()  (u8))
(define-binary-type word ()  (u16))
(define-binary-type dword () (u32))
(define-binary-type qword () (u64))

(define-binary-type signed-integer (bytes endianess)
  (:reader (in)
   (let ((value (read-value 'unsigned-integer in
			    :bytes bytes
			    :endianess endianess))
	 (bits (* 8 bytes)))
     (if (< (expt 2 (1- bits)) value)
	 value
	 (- (expt 2 bits) value))))
  (:writer (out value)
   (write-value 'unsigned-integer out (if (minusp value)
					  (+ (expt 2 (* 8 bytes)) value)
					  value)
		:endianess endianess)))

(define-binary-type s8 ()  (signed-integer :bytes 1))
(define-binary-type s16 () (signed-integer :bytes 2))
(define-binary-type s32 () (signed-integer :bytes 4))
(define-binary-type s64 () (signed-integer :bytes 8))

(define-binary-type uchar ()
  (:reader (in)
   (let ((code (read-value 'u8 in)))
     (code-char code)))
  (:writer (out value)
   (write-value 'u8 out (char-code value))))

(define-binary-literal ei-mag0 (uchar) (code-char #x7F))
(define-binary-literal ei-mag1 (uchar) #\E)
(define-binary-literal ei-mag2 (uchar) #\L)
(define-binary-literal ei-mag3 (uchar) #\F)

(define-binary-enum ei-class (u8)
  (elfclassnone
   elfclass32
   elfclass64))

(define-binary-enum ei-data (u8)
  (elfdatanone
   elfdata2lsb
   elfdata2msb))

(define-binary-enum ei-version (u8)
  (ev-none
   ev-current))

(define-binary-class e-ident ()
  ((ei-mag0		ei-mag0)
   (ei-mag1		ei-mag1)
   (ei-mag2		ei-mag2)
   (ei-mag3		ei-mag3)
   (ei-class		ei-class)
   (ei-data		ei-data)
   (ei-version		ei-version)
   (ei-osabi		u8)
   (ei-abiversion	u8)
   (ei-pad		(unsigned-integer :bytes 7))))

(defun current-endianess ()
  (ecase (ei-data (e-ident (current-binary-object)))
    (elfdata2lsb :lsb)
    (elfdata2msb :msb)))

(defun current-bytes ()
  (ecase (ei-class (e-ident (current-binary-object)))
    (elfclass32 4)
    (elfclass64 8)))

(define-binary-type elf-type-addr ()
  (unsigned-integer
   :bytes (current-bytes)
   :endianess (current-endianess)))
(define-binary-type elf-type-off ()
  (unsigned-integer
   :bytes (current-bytes)
   :endianess (current-endianess)))
(define-binary-type elf-type-half ()
  (unsigned-integer
   :bytes 2
   :endianess (current-endianess)))
(define-binary-type elf-type-word ()
  (unsigned-integer
   :bytes 4
   :endianess (current-endianess)))
(define-binary-type elf-type-sword ()
  (signed-integer
   :bytes 4
   :endianess (current-endianess)))

;; we want xword and sxword to become word and sword
;; in case of elf32
(define-binary-type elf-type-xword ()
  (unsigned-integer
   :bytes (current-bytes)
   :endianess (current-endianess)))
(define-binary-type elf-type-sxword ()
  (igned-integer
   :bytes (current-bytes)
   :endianess (current-endianess)))

(define-binary-enum e-type (elf-type-half)
  (et-none		; no file type
   et-rel		; relocatable file
   et-exec		; executable file
   et-dyn		; shared object file
   et-core		; core file
   (et-loos #xFE00)	; operating system-specific
   (et-hios #xFEFF)	; operating system-specific
   (et-loproc #xFF00)	; processor-specific
   (et-hiproc #xFFFF))) ; processor-specific

(define-binary-enum e-machine (elf-type-half)
  (em-none		; no machine
   em-m32		; AT&T WE 32100
   em-sparc		; SPARC
   em-386		; Intel 80386
   em-68k		; Motorola 68000
   em-88k		; Motorola 88000
   reserved-6
   em-860		; Intel 80860
   em-mips		; MIPS I Architecture
   em-s370		; IBM System/370 Processor
   em-mips-rs3-le	; MIPS RS3000 Little-endian
   reserved-11
   reserved-12
   reserved-13
   reserved-14
   em-parisc		; Hewlett-Packard PA-RISC
   reserved-16
   em-vpp500		; Fujitsu VPP500
   em-sparc32plus	; Enhanced instruction set SPARC
   em-960		; Intel 80960
   em-ppc		; PowerPC
   em-ppc64		; 64-bit PowerPC
   reserved-22
   reserved-23
   reserved-24
   reserved-25
   reserved-26
   reserved-27
   reserved-28
   reserved-29
   reserved-30
   reserved-31
   reserved-32
   reserved-33
   reserved-34
   reserved-35
   em-v800		; NEC V800
   em-fr20		; Fujitsu FR20
   em-rh32		; TRW RH-32
   em-rce		; Motorola RCE
   em-arm		; Advanced RISC Machines ARM
   em-alpha		; Digital Alpha
   em-sh		; Hitachi SH
   em-sparcv9		; SPARC Version 9
   em-tricore		; Siemens Tricore embedded processor
   em-arc		; Argonaut RISC Core, Argonaut Technologies Inc.
   em-h8-300		; Hitachi H8/300
   em-h8-300h		; Hitachi H8/300H
   em-h8s		; Hitachi H8S
   em-h8-500		; Hitachi H8/500
   em-ia-64		; Intel IA-64 processor architecture
   em-mips-x		; Stanford MIPS-X
   em-coldfire		; Motorola ColdFire
   em-68hc12		; Motorola M68HC12
   em-mma		; Fujitsu MMA Multimedia Accelerator
   em-pcp		; Siemens PCP
   em-ncpu		; Sony nCPU embedded RISC processor
   em-ndr1		; Denso NDR1 microprocessor
   em-starcore		; Motorola Star*Core processor
   em-me16		; Toyota ME16 processor
   em-st100		; STMicroelectronics ST100 processor
   em-tinyj		; Advanced Logic Corp. TinyJ embedded processor family
   reserved-62
   reserved-63
   reserved-64
   reserved-65
   em-fx66		; Siemens FX66 microcontroller
   em-st9plus		; STMicroelectronics ST9+ 8/16 bit microcontroller
   em-st7		; STMicroelectronics ST7 8-bit microcontroller
   em-68hc16		; Motorola MC68HC16 Microcontroller
   em-68hc11		; Motorola MC68HC11 Microcontroller
   em-68hc08		; Motorola MC68HC08 Microcontroller
   em-68hc05		; Motorola MC68HC05 Microcontroller
   em-svx		; Silicon Graphics SVx
   em-st19		; STMicroelectronics ST19 8-bit microcontroller
   em-vax		; Digital VAX
   em-cris		; Axis Communications 32-bit embedded processor
   em-javelin		; Infineon Technologies 32-bit embedded processor
   em-firepath		; Element 14 64-bit DSP Processor
   em-zsp		; LSI Logic 16-bit DSP Processor
   em-mmix		; Donald Knuth's educational 64-bit processor
   em-huany		; Harvard University machine-independent object files
   em-prism))		; SiTera Prism

(define-binary-enum e-version (elf-type-word)
  (ev-none
   ev-current))

(define-binary-class elf-header ()
  ((e-ident	e-ident)
   (e-type	e-type)
   (e-machine	e-machine)
   (e-version	e-version)
   (e-entry	elf-type-addr)
   (e-phoff	elf-type-off)
   (e-shoff	elf-type-off)
   (e-flags	elf-type-word)
   (e-ehsize	elf-type-half)
   (e-phentsize elf-type-half)
   (e-phnum	elf-type-half)
   (e-shentsize elf-type-half)
   (e-shnum	elf-type-half)
   (e-shtrndx	elf-type-half)))
