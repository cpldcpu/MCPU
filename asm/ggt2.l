SMAL32, rev  6/98.              ggt2.as                      17:20:31  Page  1
                                                             Tue Oct 16 2001

                             1  
                             2  ; GCD for TI0802 using Dijkstras Algorithm
                             3  ;
                             4  ; Optimized for speed
                             5  ;
                             6  ; 12.01.01 Tim Boescke
                             7  
                             8  USE "cpu3.inc"
 00003D: 00  FF  01          9  
                            10  start:
                            11          NOR     allone
 000000: 3E                 12          NOR     b
 000001: 14                 13          ADD     one
 000002: 7F                 14          STA     bneg    ; bneg=-b
 000003: 95                 15  
                            16  ;----------- INNERLOOP --------------
                            17  loop:
                            18          NOR     allone  ;Akku = 0
 000004: 3E                 19          ADD     bneg
 000005: 55                 20  cont:
                            21          ADD     a       ;Akku = a - b
 000006: 53                 22  
                            23          JCC     neg     ;jump when Akku<0
 000007: CC                 24  
                            25          STA     a
 000008: 93                 26  
                            27          ADD     allone
 000009: 7E                 28          JCC     end     ;A=0 ? -> end,  result in b
 00000A: CE                 29                  
                            30          JCC     loop    ;uncond.
 00000B: C4                 31  neg:
                            32          STA     bneg
 00000C: 95                 33          JCC     cont    ;uncond.
 00000D: C6                 34  
                            35  ;------------------------------------
                            36  
                            37  end:
                            38                          ;Akku contains $FF here
                            39          ADD     bneg     
 00000E: 55                 40          NOR     zero
 00000F: 3D                 41          STA     b       ; b=-bneg
 000010: 94                 42  wait:
                            43          JCC     wait
 000011: D1                 44          JCC     wait
 000012: D1                 45  a:
                            46          DCB     (6)
 000013: 06                 47  b:
                            48          DCB     (4)
 000014: 04                 49  bneg:   
                            50          DCB     (0)
 000015: 00                 51  END
                    no errors
