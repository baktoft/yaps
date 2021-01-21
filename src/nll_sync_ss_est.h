	// PARAMETER(logD_v);    		// Diffusivity of sound speed
	// Type D_v = exp(logD_v);

	PARAMETER_VECTOR(SS);		// Estimated speed of sound


	for(int i = 0; i < n_ss_idx; ++i){
		nll -= dnorm(SS(i), Type(1475.0), Type(100), true);
	}

	for(int i = 0; i < np; i++){
		ss_i(i) = SS(ss_idx(i));
	}

	// nll -= dnorm(ss(0),Type(1475.0),Type(100.0),true);		
	// for(int i = 1; i < n_ss; ++i){
		// nll -= dnorm(ss(i), ss(i-1),sqrt(2*D_v), true);
		// nll -= dnorm(ss(i),Type(1475.0),Type(100.0),true);		
	// }
	
