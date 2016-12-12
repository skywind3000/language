love(zhangxueyou,wanfei).
love(zhangxueyou,zouhuimin).
love(wanfei,xietinfen).
love(zouhuimin,zhangxueyou).
love(xietinfen,wanfei).
love(xietinfen,zouhuimin).
love(liudehua,zouhuimin).
lovers(X,Y):-love(X,Y),love(Y,X).
rival_in_love(X,Y):-love(X,Z),\+(love(Z,X)),love(Z,Y).

