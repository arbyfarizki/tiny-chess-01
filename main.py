import json,os,urllib.request as u
V=[0,100,320,330,500,900,0]
M=-100001
C=[0,0,6,4,0,2,-2]
N=[33,31,18,14,-14,-18,-31,-33];D=[15,17,-15,-17];T=[1,-1,16,-16];G=T+D;R=(5,4,3,2);H=[0,0,N,D,T,G,G]
K=[15]*128;K[0]=13;K[7]=14;K[4]=12;K[112]=7;K[119]=11;K[116]=3
B=[0]*128;Q=[0,15,-1,0];J=[-1,-1];U=[];W=[0]
def X(p,q):
 t=p&7;c=p>>3;r=q>>4;f=q&7;a=r+7*c-2*r*c
 return(V[t]+(a*a if t==1 else(min(f,7-f)+min(r,7-r))*C[t]))*(1-2*c)
def ST():
 B[:]=[0]*128;Q[:]=0,15,-1,0;J[:]=4,116;del U[:]
 for f,q in enumerate(map(int,"42356324")):
  B[f]=q;B[112+f]=q+8;B[16+f]=1;B[96+f]=9
def AT(q,y):
 b=B;e=y<<3
 for d in((-15,-17),(15,17))[y]:
  t=q+d
  if not t&0x88 and b[t]==(1|e):return 1
 for p in(2,3,4,6):
  for g in H[p]:
   t=q+g
   for _ in range(7 if 2<p<6 else 1):
    if t&0x88:break
    v=b[t]
    if v:
     if v==(p|e) or(2<p<5 and v==(5|e)):return 1
     break
    t+=g
 return 0
def CS(o):
 v=Q[0];w=v^1;a=112*v;k=a+4
 if J[v]!=k or AT(k,w):return
 for b,e,t in((1,(5,6),6),(2,(3,2,1),2)):
  if Q[1]&(b<<(v*2))and not any(B[a+x]for x in e)and not any(AT(a+x,w)for x in e[:2]):o.append(k|((a+t)<<7)|(1<<19))
def GN(c=0):
 b=B;v=Q[0];w=v^1;o=[];d=16-32*v
 for f in range(128):
  p=b[f]
  if f&0x88 or not p or(p>>3)!=v:continue
  y=p&7
  if y==1:
   r=f>>4;e=f+d
   if not b[e]:
    if r==6-5*v:
     for q in R:o.append(f|(e<<7)|(q<<14))
    elif not c:
     o.append(f|(e<<7))
     if r==1+5*v and not b[e+d]:o.append(f|((e+d)<<7)|(1<<20))
   for g in(d-1,d+1):
    q=f+g
    if q&0x88:continue
    x=b[q]
    if x and(x>>3)==w:
     if r==6-5*v:
      for j in R:o.append(f|(q<<7)|(j<<14)|(1<<17))
     else:o.append(f|(q<<7)|(1<<17))
    elif q==Q[2]:o.append(f|(q<<7)|(1<<17)|(1<<18))
   continue
  for g in H[y]:
   q=f
   for _ in range(7 if 2<y<6 else 1):
    q+=g
    if q&0x88:break
    x=b[q]
    if not x and not c or x and(x>>3)==w:o.append(f|(q<<7)|((x!=0)<<17))
    if x:break
 if not c:CS(o)
 return o
def MK(m):
 b=B;f=m&127;t=(m>>7)&127;j=(m>>14)&7;g=m>>17;p=b[f];v=Q[0]
 s=t-16+32*v if g&2 else t
 z=b[s]
 U.append((m,z,s,Q[1],Q[2],Q[3]))
 Q[3]-=X(p,f);b[f]=0
 if z:Q[3]-=X(z,s);b[s]=0
 w=j|v<<3 if j else p;b[t]=w;Q[3]+=X(w,t)
 if p&7==6:J[v]=t
 if g&4:
  e=f+3 if t>f else f-4;k=e-2 if t>f else e+3;r=b[e];Q[3]+=X(r,k)-X(r,e);b[e]=0;b[k]=r
 Q[1]&=K[f]&K[t];Q[2]=-1
 if g&8:
  e=(f+t)//2;a=e&7;y=(1|(v^1)<<3)
  if(a and b[t-1]==y)or(a<7 and b[t+1]==y):Q[2]=e
 Q[0]^=1
 if AT(J[v],v^1):UM();return 0
 return 1
def UM():
 m,z,s,a,e,w=U.pop();f=m&127;t=(m>>7)&127;j=(m>>14)&7;g=m>>17;v=Q[0]^1;o=1|v<<3 if j else B[t];B[t]=0
 if g&4:
  a2=f+3 if t>f else f-4;k=a2-2 if t>f else a2+3;B[a2]=B[k];B[k]=0
 B[f]=o
 if o&7==6:J[v]=f
 if z:B[s]=z
 Q[0]=v;Q[1]=a;Q[2]=e;Q[3]=w
def OD(o):
 return sorted(o,key=lambda m:-((999+V[(m>>14)&7])if(m>>14)&7 else(99+10*V[(B[(m>>7)&127]&7)or 1]-V[B[m&127]&7])if m>>17&1 else 0))
def QS(a,b,y):
 W[0]+=1
 if W[0]>99999:raise Exception
 e=Q[3]*(1-2*Q[0])
 c=AT(J[Q[0]],Q[0]^1);s2=M
 if not c:
  if e>=b:return e
  s2=e
  a=max(a,e)
 o=OD(GN(not c))
 for m in o:
  if not MK(m):continue
  s=-QS(-b,-a,y+1)
  UM()
  s2=max(s2,s)
  if s>=b:return s
  a=max(a,s)
 return s2
def NM(d,a,b,y):
 if d<=0:return QS(a,b,y)
 W[0]+=1
 if W[0]>99999:raise Exception
 o=OD(GN());s2=M
 for m in o:
  if not MK(m):continue
  s=-NM(d-1,-b,-a,y+1)
  UM()
  s2=max(s2,s)
  a=max(a,s)
  if a>=b:break
 if s2==M:return M+1+y if AT(J[Q[0]],Q[0]^1) else 0
 return s2
def RT():
 o=[m for m in GN() if MK(m) and not UM()]
 o=OD(o);bm=o[0];W[0]=0
 try:
  for d in range(8):
   a=M
   for m in o:
    MK(m);s=-NM(d,M,-a,1);UM()
    if s>a:a=s;bm=m
 except:0
 return bm
def UC(m):
 f=m&127;t=(m>>7)&127;j=(m>>14)&7
 s=chr(97+(f&7))+str((f>>4)+1)+chr(97+(t&7))+str((t>>4)+1)
 if j:s+='nbrq'[j-2]
 return s
def CH(mv):
 ST()
 for s in mv:
  if not(m:=next((x for x in GN() if UC(x)==s),None)):break
  MK(m)
 return UC(RT())
def PO(a,b):
 try:return json.load(u.urlopen(os.getenv("MAKEFUN_ENV_URL")+a,json.dumps(b).encode()))
 except:exit(1)
while(g:=PO("/game",{}))["status"]!="done":
 while g["status"]=="your_move":
  m=CH(g["moves"])
  g=PO("/move",{"game":g["game"],"move":m})