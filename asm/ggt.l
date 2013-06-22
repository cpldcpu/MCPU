SMAL32, rev  6/98.              ggt.as                       22:38:29  Page  1
                                                             Fri Jan 12 2001

                             1  
                             2  ; GCD for CPU3 using Dijkstras Algorithm
                             3  ;
                             4  ;
                             5  ;
                             6  ; 12.01.01 Tim Boescke
                             7  
                             8  USE "cpu3.inc"
 00003D: 00  FF  01          9  
                            10  start:
                            11          NOR     allone  ;Akku = 0
 000000: 3E                 12          NOR     b
 000001: 0F                 13          ADD     one     ;Akku = - b
 000002: 7F                 14  
                            15          ADD     a       ;Akku = a - b
 000003: 4E                 16                          ;Carry set when akku >= 0
                            17  
                            18          JCC     neg
 000004: C9                 19  
                            20          STA     a
 000005: 8E                 21  
                            22          ADD     allone
 000006: 7E                 23          JCC     end     ;A=0 ? -> end,  result in b
 000007: CD                 24  
                            25          JCC     start
 000008: C0                 26  neg:
                            27          NOR     zero
 000009: 3D                 28          ADD     one     ;Akku = -Akku
 00000A: 7F                 29  
                            30          STA     b
 00000B: 8F                 31          JCC     start   ;Carry was not altered
 00000C: C0                 32  
                            33  end:
                            34          JCC     end
 00000D: CD                 35  
                            36  a:
                            37          DCB     (126)
 00000E: 7E                 38  b:
                            39          DCB     (124)
 00000F: 7C                 40  
                            41  bneg:   
                            42          DCB     (0)
 000010: 00                 43  END
                    no errors
