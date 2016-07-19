#mklisper
Common LISP utility library for everyday developer <br>
Mark Yang <br>
(mkyang@mediamtd.com)

##Introduction
This is code sniffet used for several my projects. I am using Franz Allegro CL on Microsoft Windows
So all codes are heavily depend on win32.
I hope some of these code can solve your problems.

##Enviroment
- OS : Microsoft Windows
- Compiler : Franz Allegro CL

##File Description
- mkCRC : CRC32 generator
- mkdib : win32 DIB(Device Independant Bitmap) wrapper for CL
- mkqueue : thread safe queue based on mp:queue
- mkthread : run lisp functions in OS native thread. It's performance almost same as C++ thread control
- mkutil : MISC utility sniffet
