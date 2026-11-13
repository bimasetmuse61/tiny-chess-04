import json,os,urllib.request as R
MV=[0,100,300,310,500,950,0]
X="aaaaijkkefggbcdeabcdaabc9abbaaaa468868aa8acd8bde8bde8acd68ab468842204220422064428664a886cee8aff9"
D=[(int(c,36)-10)*5 for c in X]
Z=[0]*32
TS=[D[:32],D[32:64],Z,Z,Z,D[64:]]
PC={1:"PNBRQK",-1:"pnbrqk"}
def _mk(tab):
 V={}
 for t in range(1,7):
  v=tab[t-1];w=[0]*128
  for sq in range(128):
   if not sq&0x88:w[sq]=MV[t]+v[(7-(sq>>4))*4+min(sq&7,7-(sq&7))]
  V[PC[1][t-1]]=w;V[PC[-1][t-1]]=[-w[sq^112] for sq in range(128)]
 return V
VC=_mk(TS)
C2T={c:(i%6+1)*(1 if i<6 else -1) for i,c in enumerate("PNBRQKpnbrqk")}
ND=[-33,-31,-18,-14,14,18,31,33]
BD=[-17,-15,15,17]
RD=[-16,-1,1,16]
KD=BD+RD
DL=[0,0,ND,BD,RD,KD,KD]
CM={0:13,7:14,4:12,112:7,119:11,116:3}
INF=10**7;BDGT=9e4;TT={};NDN=0
def att(B,sq,by):
 pc=PC[by]
 for ds,t,r in ((ND,1,0),(KD,5,0),(BD,2,1),(RD,3,1),(KD,4,1),((-15*by,-17*by),0,0)):
  for d in ds:
   s=sq+d
   while not s&0x88:
    p=B[s]
    if p!=".":
     if p==pc[t]:return 1
     break
    if not r:break
    s+=d
 return 0
def gen(st,cap=False):
 B,stm,CR=st[:3]
 ms=[];ap=ms.append;pc=PC[stm]
 for sq,p in enumerate(B):
  if p not in pc:continue
  t=abs(C2T[p])
  if t==1:
   d=16*stm;one=sq+d
   if B[one]=="." and (not cap or one>>4 in (0,7)):
    if one>>4 in (0,7):
     for pr in (5,4,3,2):ap(sq|one<<7|pr<<14)
    else:
     ap(sq|one<<7)
     if sq>>4==1+5*(stm<0) and B[sq+2*d]==".":ap(sq|(sq+2*d)<<7)
   for cd in (d-1,d+1):
    to=sq+cd
    if to&0x88:continue
    q=B[to]
    if q!="." and q not in pc:
     for pr in ((5,4,3,2) if to>>4 in (0,7) else (0,)):ap(sq|to<<7|pr<<14)
  else:
   if t==6 and not cap:
    e=4+112*(stm<0);bk=1+3*(stm<0)
    for df,b,em in ((2,bk,(1,2)),(-2,bk*2,(-1,-2,-3))):
     if sq==e and CR&b and all(B[e+x]=="." for x in em) and not any(att(B,e+x,-stm) for x in (0,df//2,df)):ap(e|(e+df)<<7)
   for d in DL[t]:
    to=sq+d
    while not to&0x88:
     q=B[to]
     if q==".":
      if not cap:ap(sq|to<<7)
     else:
      if q not in pc:ap(sq|to<<7)
      break
     if t==2 or t==6:break
     to+=d
 return ms
def apply(st,m):
 B,stm,CR,S=st
 f=m&127;t=(m>>7)&127;pr=(m>>14)&7
 pc=B[f];cap=B[t]
 B=B[:f]+"."+B[f+1:];S-=VC[pc][f]
 if cap!=".":B=B[:t]+"."+B[t+1:];S-=VC[cap][t]
 npc=PC[stm][pr-1] if pr else pc
 B=B[:t]+npc+B[t+1:];S+=VC[npc][t]
 if pc in "Kk" and abs(t-f)==2:
  rf,rt=(f+3,f+1) if t>f else (f-4,f-1);rp=B[rf]
  B=B[:rf]+"."+B[rf+1:];B=B[:rt]+rp+B[rt+1:];S+=VC[rp][rt]-VC[rp][rf]
 return B,-stm,CR&CM.get(f,15)&CM.get(t,15),S
def set_fen(fen):
 p=fen.split();B=["."]*128;S=0;sq=112
 for ch in p[0]:
  if ch=="/":sq-=24
  elif ch.isdigit():sq+=int(ch)
  else:
   B[sq]=ch;S+=VC[ch][sq];sq+=1
 CR=sum(1<<"KQkq".index(ch) for ch in p[2] if ch in "KQkq")
 return "".join(B),1 if p[1]>"b" else -1,CR,S
def neg(st,d,a,b):
 global NDN
 NDN+=1
 if NDN>BDGT:raise Exception()
 B,stm,CR,S=st
 kpc="Kk"[stm<0]
 inch=att(B,B.find(kpc),-stm)
 best=-INF
 if d<=0 and not inch:
  best=S*stm
  if best>=b:return best
  a=max(a,best)
 tm=0
 if d>0:
  e=TT.get(st[:3])
  if e:
   tm=e[2]
   if e[0]>=d:return e[1]
 ms=gen(st,d<=0 and not inch)
 ms.sort(key=lambda m:(m==tm,MV[abs(C2T[v])] if (v:=B[(m>>7)&127])!="." else 0),reverse=True)
 bm=0
 for m in ms:
  cs=apply(st,m)
  if att(cs[0],cs[0].find(kpc),-stm):continue
  if bm:
   v=-neg(cs,d-1,-a-1,-a)
   if a<v<b:v=-neg(cs,d-1,-b,-a)
  else:v=-neg(cs,d-1,-b,-a)
  if v>best:best=v;bm=m
  a=max(a,v)
  if a>=b:break
 if not bm:return -INF+100 if inch else 0 if d>0 else best
 if d>0:TT[st[:3]]=(d,best,bm)
 return best
def root(st):
 global NDN
 NDN=0;bm=0
 for d in range(1,99):
  try:neg(st,d,-INF,INF)
  except Exception:break
  bm=TT.get(st[:3],(0,0,bm))[2]
 return bm
def uci(m):
 return chr(97+(m&7))+str((m>>4&7)+1)+chr(97+(m>>7&7))+str((m>>11&7)+1)+("nbrq"[(m>>14)-2] if m>>14 else "")
def post(p,b):
 q=R.Request(E+p,data=json.dumps(b).encode())
 return json.loads(R.urlopen(q).read())
if __name__=="__main__":
 E=os.environ["MAKEFUN_ENV_URL"]
 while (g:=post("/game",{}))["status"]!="done":
  while g["status"]=="your_move":g=post("/move",{"game":g["game"],"move":uci(root(set_fen(g["fen"])))})