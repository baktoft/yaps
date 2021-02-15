// nll contrib for top when pingType == 'sbi_double'
	DATA_SCALAR(approxBI_main);
	DATA_SCALAR(approxBI_sub);

	PARAMETER(logSigma_bi);		// Sigma for burst interval
	Type sigma_bi = exp(logSigma_bi);

	nll -= dnorm(top(0),Type(0.0),Type(4.0),true);
	nll -= dnorm(top(1),Type(approxBI_sub),Type(4.0),true);
	nll -= dnorm(top(2),Type(approxBI_sub + approxBI_main),Type(4.0),true);
	nll -= dnorm(top(3),Type(approxBI_sub + approxBI_main + approxBI_sub),Type(4.0),true);

	for(int i = 4; i < np; ++i){
		nll -= dnorm(top(i)-2*top(i-2)+top(i-4), Type(0), sigma_bi, true);
	}


	// for(int i = 2; i < np; ++i)	{
	// 	if(i%2 == 0){ //even i - next ping is 'sub' seconds away
	// 		nll -= dnorm(top(i)-2*top(i-1)+top(i-2) + approxBI_main - approxBI_sub, Type(0), sigma_bi, true);
	// 	} else { //uneven i - next ping is 'main' seconds away
	// 		nll -= dnorm(top(i)-2*top(i-1)+top(i-2) - approxBI_main + approxBI_sub, Type(0), sigma_bi, true);
	// 	}
	// 	//nll -= dnorm(top(i)-2*top(i-1)+top(i-2), Type(0), sigma_bi, true);
	// 	//nll += bi_penalty * (softplus(0 - (top(i) - top(i-1)), bi_epsilon));
	// }
