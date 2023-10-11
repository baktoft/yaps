	PARAMETER(LOG_D_SS);    		// Diffusivity of sound speed
	Type D_SS = exp(LOG_D_SS);

	PARAMETER_VECTOR(SS);		// Estimated speed of sound


	// for(int i = 0; i < n_offset_idx; ++i){
		// nll -= dnorm(SS(i), Type(1475.0), Type(100), true);
		// nll += Type(1E6) * (softplus(SS(i) - Type(1580), Type(1E-6)) + softplus(Type(1380) - SS(i), Type(1E-6)));
	// }

	nll -= dnorm(SS(0),Type(1475.0),Type(100.0),true);		
	nll += Type(1E6) * (softplus(SS(0) - Type(1580), Type(1E-6)) + softplus(Type(1380) - SS(0), Type(1E-6)));
	
	for(int i = 1; i < n_offset_idx; ++i){
		nll -= dnorm(SS(i), SS(i-1),sqrt(2*D_SS), true);
		nll += Type(1E6) * (softplus(SS(i) - Type(1580), Type(1E-6)) + softplus(Type(1380) - SS(i), Type(1E-6)));
	}

	for(int i = 0; i < np; i++){
		ss_i(i) = SS(offset_idx(i));
	}

	
