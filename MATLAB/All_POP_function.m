%Code separates those into younger/older than 20
%after the fair ends
function f = All_POP_function(t,y,pars)

%% pig model
S=y(1);
E=y(2);
A=y(3);
I=y(4);
R=y(5);
C=y(6);

%% Human equations %%%
%Younger than 20 human population
Sy=y(7);
Ey=y(8);
Iy=y(9);
Ry=y(10);

%Older than 20 human population
So=y(11);
Eo=y(12);
Io=y(13);
Ro=y(14);

%cumulative infection for human attendee population
Ca=y(15);

%Member population
Sm=y(16);
Em=y(17);
Im=y(18);
Rm=y(19);

Cm=y(20);

%human equations;
beta=pars(1); % force of infection
gammaS = pars(2); % recovery rate for swine
kappaS = pars(3); % latency for swine
delta = pars(4); % proportion asymptomatic
N = pars(5); % total swine population
Ca = pars(6); % contact duration (minutes)
Pa = pars(7); % probability of transmission ( per min)
kappaH = pars(8); % latency for humans
gammaH = pars(9); % recovery rate for humans

Cm = pars(10); %contact duration for the members
Pm = pars(11); %prob of transmission for members

f=zeros(20,1);
if t>8
   f(1)=0; %S
   f(2)= -kappaS*E; %E
   f(3)= kappaS*(delta)*E-gammaS*A; %A
   f(4)= kappaS*(1-delta)*E-gammaS*I; %I
   f(5)= gammaS*I + gammaS*A; %R
   f(6)= 0; %C

   f(7)=-Ca*Pa*Sy*(0/N); %Sy
   f(8)= Ca*Pa*Sy*(0/N) - kappaH*Ey; %Ey
   f(9)= kappaH*Ey - gammaH*Iy; %Iy
   f(10)= gammaH*Iy; %Ry
   f(11)= -Ca*Pa*So*(0/N); %So
   f(12)= Ca*Pa*So*(0/N) - kappaH*Eo; %Eo
   f(13)= kappaH*Eo -gammaH*Io; %Io
   f(14)= gammaH*Io; %Ro

   f(15)= kappaH*Ey + kappaH*Eo; %Ca

   f(16)= -Cm*Pm*Sm*(0/N); %Sm
   f(17)= Cm*Pm*Sm*(0/N) - kappaH*Em; %Em
   f(18)= kappaH*Em -gammaH*Im; %Im
   f(19)= gammaH*Im; %Rm

   f(20)= kappaH*Em; %Cm
else
   f(1)=-beta*S*((I+A)/N); %S
   f(2)= beta*S*((I+A)/N)-kappaS*E; %E
   f(3)= kappaS*delta*E -gammaS*A; %A
   f(4)= kappaS*(1-delta)*E -gammaS*I; %I
   f(5)= gammaS*I+gammaS*A; %R
   
   f(6)= beta*S*((I+A)/N); %C

   f(7)=-Ca*Pa*Sy*((I+A)/N); %Sy
   f(8)= Ca*Pa*Sy*((I+A)/N) - kappaH*Ey; %Ey
   f(9)= kappaH*Ey - gammaH*Iy; %Iy
   f(10)= gammaH*Iy; %Ry

   f(11)= -Ca*Pa*So*((I+A)/N); %So
   f(12)= Ca*Pa*So*((I+A)/N) - kappaH*Eo; %Eo
   f(13)= kappaH*Eo -gammaH*Io; %Io
   f(14)= gammaH*Io; %Ro

   f(15)= kappaH*Ey + kappaH*Eo; %Ca

   f(16)= -Cm*Pm*Sm*((I+A)/N); %Sm
   f(17)= Cm*Pm*Sm*((I+A)/N) - kappaH*Em; %Em
   f(18)= kappaH*Em -gammaH*Im; %Im
   f(19)= gammaH*Im; %Rm

   f(20)= kappaH*Em; %Cm
end
end
