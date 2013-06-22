SMAL32, rev  6/98.              prime.as                     15:23:35  Page  1
                                                             Sun Jan 28 2001

                             1  
                             2  ; Primetester
                             3  ;
                             4  ;
                             5  ;
                             6  ; 27.01.01 Tim Boescke
                             7  
                             8  USE "cpu3.inc"
 00003D: 00  FF  01          9  
                            10  ;       sub=-2; 
                            11  ;       while ((number+sub)>0)
                            12  ;       {
                            13  ;               test=number;
                            14  ;               while (test>0) test+=sub;
                            15  ;               if (test==0) return 0;
                            16  ;               sub+=-1;
                            17  ;       }
                            18  ;       return 1;
                            19  
                            20  start:
                            21  
                            22          NOR     allone
 000000: 3E                 23          ADD     allone
 000001: 7E                 24          ADD     allone
 000002: 7E                 25          STA     sub     ;sub=-2
 000003: 9F                 26  
                            27  loop:
                            28          NOR     allone
 000004: 3E                 29          ADD     number
 000005: 60                 30  innerloop:
                            31          ADD     sub     ;
 000006: 5F                 32          JCC     out
 000007: C9                 33          JCC     innerloop
 000008: C6                 34  out:
                            35          NOR     zero
 000009: 3D                 36          ADD     one
 00000A: 7F                 37          ADD     sub
 00000B: 5F                 38  
                            39          ADD     allone
 00000C: 7E                 40          JCC     noprime
 00000D: D9                 41  
                            42          NOR     allone
 00000E: 3E                 43          ADD     sub
 00000F: 5F                 44          ADD     allone
 000010: 7E                 45          STA     sub     ;sub-=1 
 000011: 9F                 46                          
                            47          ADD     allone
 000012: 7E                 48          ADD     number
 000013: 60                 49          JCC     prime
 000014: D6                 50          JCC     loop
 000015: C4                 51  
                            52  prime:
                            53          NOR     allone
 000016: 3E                 54          ADD     number  ;load prime number
 000017: 60                 55  
SMAL32, rev  6/98.              prime.as                     15:23:35  Page  2
                                                             Sun Jan 28 2001

                            56                          ; Add displaycode here
                            57                          ; (Or breakpoint)
                            58          JCC     next
 000018: D9                 59  
                            60  noprime:
                            61  next:
                            62          NOR     allone
 000019: 3E                 63          ADD     number
 00001A: 60                 64          ADD     two
 00001B: 5E                 65          STA     number
 00001C: A0                 66  
                            67          JCC     start
 00001D: C0                 68  
                            69  two:
                            70          DCB     (2)
 00001E: 02                 71  sub:
                            72          DCB     (0)
 00001F: 00                 73  
                            74  number:
                            75          DCB     (3)
 000020: 03                 76  END
                    no errors
