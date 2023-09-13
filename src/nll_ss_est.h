	PARAMETER(logD_v);    		// Diffusivity of sound speed
	Type D_v = exp(logD_v);
	nll -= dnorm(logD_v, Type(0), Type(5), true);
	
	PARAMETER_VECTOR(SS);		// Estimated speed of sound

	// nll -= dnorm(SS(0),Type(1475.0),Type(100.0),true);		
	for(int i = 0; i < n_ss; ++i){
		nll -= dnorm(SS(i),Type(1475.0),Type(100.0),true);	
	}
	for(int i = 1; i < n_ss; ++i){
		nll -= dnorm(SS(i), SS(i-1),sqrt(2*D_v), true);
	}
	for(int i = 0; i < n_ss; ++i){
		nll += Type(1E6) * (softplus(SS(i) - 1600, Type(1E-6)) + softplus(1400 - SS(i), Type(1E-6)));
	}
	
	for(int i = 0; i < np; i++){
		ss_i(i) = SS(ss_idx(i));
	}
