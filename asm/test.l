SMAL32, rev  6/98.              test.as                      02:17:50  Page  1
                                                             Fri Jan 12 2001

                             1  
                             2  ;23.09.2000 Tim Böscke
                             3  
                             4  USE "cpu3.inc"
 00003D: 00  FF  01          5  
                             6  label0:
                             7          LDA     count
 000000: 3E  46              8  cloop:  
                             9          ADD     one     
 000002: 7F                 10          JCC     cloop
 000003: C2                 11  loop:
                            12          JMP loop
 000004: C4  C4             13  
                            14  count:  
                            15          DCB     (256-10)
 000006: F6                 16  END
                    no errors
