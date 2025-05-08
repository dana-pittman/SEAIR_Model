%% Simulation for
%parameter values;
   beta= 6*1/5; % force of infection
   gammaS= 1/5; % recovery rate
   kappaS= 1/2; % latent period
   delta = 0.83;  % proportion subclincal
   N=208; % total swine population
   Ca = 5; % contact per min attendee
   Pa = 0.002; % prob of transmission attendee
   kappaH = 1/2; % latent period
   gammaH = 1/5; % recovery
   Cm = 60; % contact per mminute member
   Pm=0.00356; % prob of tranmission member

tspan=[0:14];
% Wong et al 2013 cites 10% of younger than 20 were immune and 50% of older
% than 20 were immune
y0=[203;0;5;0;0;0;5821;0;0;647;4221;0;0;4221;0;90;0;0;10;0];

pars=[beta, gammaS, kappaS, delta, N, Ca, Pa, kappaH, gammaH, Cm, Pm];

[t,y]=ode45(@All_POP_function,tspan,y0,[],pars);
X= y(2:15,15)-y(1:14,15);

% Plot cumulative infections over time
figure
plot(t, y(:,6), 'red'); %cumulative pig
hold on
plot(t, y(:,15), 'black'); %cumulative attendee
hold on
plot(t, y(:,20), 'blue'); % cumulative member
hold on

data = [ 0; 0; 0; 3;14;25;37;44;54;67;67;68;70;72;72];
scatter(t,data, 'x', 'k')
hold on

mdata= [ 0; 0; 0; 0; 1; 4; 6; 9; 9; 9;12;13;13;13;13];
scatter(t,mdata, 'o', 'b');
legend({'Swine', 'Attendee','Member','Observed Attendee', 'Observed Member'}, 'Location','northwest')
xlabel('Time');
ylabel('Cumulative Infections');
title('Cumulative Number of Infections Over Time (R0=6)');
