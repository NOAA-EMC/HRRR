program unittest01
  implicit none
  integer,parameter:: iromb=0,maxwv=7
  integer,parameter:: idrtg=4,idrte=0,imax=16,jmaxg=8,jmaxe=17
  call tests(iromb,maxwv,idrtg,imax,jmaxg)
  call tests(iromb,maxwv,idrte,imax,jmaxe)
  call testv(iromb,maxwv,idrtg,imax,jmaxg)
  call testv(iromb,maxwv,idrte,imax,jmaxe)
  call tests(0,126,4,256,128)
  call tests(0,126,0,256,257)
  call testv(0,126,4,256,128)
  call testv(0,126,0,256,257)
end program
subroutine tests(iromb,maxwv,idrt,imax,jmax)
  implicit none
  integer,intent(in):: iromb,maxwv,idrt,imax,jmax
  real wave((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
  real wave2((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
  real grid(imax,jmax)
  wave=1
  wave(2:2*maxwv+2:2)=0
  call sptez(iromb,maxwv,idrt,imax,jmax,wave,grid,+1)
  call sptez(iromb,maxwv,idrt,imax,jmax,wave2,grid,-1)
  print *,sqrt(sum((wave2-wave)**2)/size(wave))/epsilon(wave)
end subroutine
subroutine testv(iromb,maxwv,idrt,imax,jmax)
  implicit none
  integer,intent(in):: iromb,maxwv,idrt,imax,jmax
  real waved((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
  real wavez((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
  real waved2((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
  real wavez2((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
  real gridu(imax,jmax)
  real gridv(imax,jmax)
  waved=1
  waved(2:2*maxwv+2:2)=0
  waved(1)=0
  wavez=1
  wavez(2:2*maxwv+2:2)=0
  wavez(1)=0
  call sptezv(iromb,maxwv,idrt,imax,jmax,waved,wavez,gridu,gridv,+1)
  call sptezv(iromb,maxwv,idrt,imax,jmax,waved2,wavez2,gridu,gridv,-1)
  print *,sqrt((sum((waved2-waved)**2)+sum((wavez2-wavez)**2))/(2*size(waved)))/epsilon(waved)
end subroutine
