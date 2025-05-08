%function fit_attendee_incidence

clf
clear

P0=linspace(0.001, 0.04, 100);
options=optimset('MaxIter', 5000, 'TolFun', 1e-8, 'TolX', 1e-8); % fine tunning

OP = zeros(length(P0), 2); % Preallocating storage for results

for k=1:length(P0)
[theta_hat,ess]=fminsearch(@error_sum_of_squares5,P0(k),options); 
OP(k,:)=[theta_hat,ess]; %stores value into op
end  

k1 = find(OP(:,2) == min(OP(:,2))); % Finding best-fit parameter

Pm=OP(k1,1);
  
   % integrate ODE for best fitting parameter values, so we can plot it
   pars=[1.2, 0.2, 0.5, 0.83, 208, 5, 0.0172, 0.5, 0.2, 60, Pm];
  
   tspan=[0:19];	 
   y0=[203;0;5;0;0;0;5821;0;0;647;4221;0;0;4221;0;90;0;0;10;0]; % take initial conditions
  
   [t,y]=ode45(@All_POP_function,tspan,y0,[],pars);

   X = y(:, 20); 
   
   %PLOTTING THE FIT
   figure(1)
   plot([0:19],X)
   hold on
   data= [ 0; 0; 0; 0; 1; 4; 6; 9; 9; 9;12;13;13;13;13;13;13;13;13;13];
   data_times=[0:19]; % data points are at t=0, 1, ... , 13
   plot(data_times,data,'x')
   hold off

   fprintf('Best-fit value for P: %.5f\n', Pm); % print out the best-fit

%end
function ESS=error_sum_of_squares5(input_pars)
  beta = 6*1/5;  % force of infection
  gammaS = 1/5;  % recovery rate
  kappaS = 1/2;  % latent period
  delta = 0.83;  % proportion subclincal
  N = 208;       % total swine population
  Ca = 5;        % contact minutes attendee
  Pa = 0.0172;   % probability of transmission attendee
  kappaH = 1/2;  % latent period
  gammaH = 1/5;  % human recovery rate
  Cm = 60;       % contact minutes member

   Pm=input_pars(1); % prob of tranmission member
   pars=[beta, gammaS, kappaS, delta, N, Ca, Pa, kappaH, gammaH, Cm, Pm];
  
   tspan=[0:19];	  % 20 days of data, including initial value
   y0=[203;0;5;0;0;0;5821;0;0;647;4221;0;0;4221;0;90;0;0;10;0]; % initial conditions 
  
   [t,y]=ode45(@All_POP_function,tspan,y0,[],pars);
   
   % outbreak data
   data= [ 0; 0; 0; 0; 1; 4; 6; 9; 9; 9;12;13;13;13;13;13;13;13;13;13];
      
    X= y(:,20);

        diff=data-X;
   
   ESS=sum(diff.^2);  % square entries of diff and then sum
   % note: there are 14 terms in this sum, but one (the first) is zero
end
