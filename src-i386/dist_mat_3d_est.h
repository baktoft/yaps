		PARAMETER_VECTOR(Z);	//Position at time of ping
	
		nll -= dnorm(Z(0),Type(0),Type(10000),true);
		
		for(int i=1; i<np; ++i)	{
			nll -= dnorm(Z(i), Z(i-1),sqrt(2*D_xy*(top(i) - top(i-1))),true);
		}
		
		for(int i=0; i<np; ++i){ //iterate pings
			for(int h=0; h<nh; ++h){ //iterate hydros
				if(!isNA(toa(h,i))){ //ignore NA's...
					dist(h,i) = sqrt((H(h,0)-X(i))*(H(h,0)-X(i)) + (H(h,1)-Y(i))*(H(h,1)-Y(i)) + (H(h,2)-Z(i))*(H(h,2)-Z(i)));
					mu_toa(h,i) = top(i) +  dist(h,i)/ss_i(i);
					eps(h,i) = toa(h,i) - mu_toa(h,i);
				}
			}
		}
