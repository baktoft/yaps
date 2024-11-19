		PARAMETER_VECTOR(Z);	//Position at time of ping
		PARAMETER(logD_z);    		// Diffusivity of z
		Type D_z = exp(logD_z);

	
		nll -= dnorm(Z(0),Type(-10),Type(200),true);
		
		for(int i=1; i<np; ++i)	{
			nll -= dnorm(Z(i), Z(i-1),sqrt(2*D_z*(TOP(i) - TOP(i-1))),true);
		}
		
		for(int i=0; i<np; ++i){ //iterate pings
			for(int h=0; h<nh; ++h){ //iterate hydros
				if(!isNA(toa(i,h))){ //ignore NA's...
					dist(i,h) = sqrt((H(h,0)-X(i))*(H(h,0)-X(i)) + (H(h,1)-Y(i))*(H(h,1)-Y(i)) + (H(h,2)-Z(i))*(H(h,2)-Z(i)));
					mu_toa(i,h) = TOP(i) +  dist(i,h)/ss_i(i);
					eps(i,h) = toa(i,h) - mu_toa(i,h);
				}
			}
		}
