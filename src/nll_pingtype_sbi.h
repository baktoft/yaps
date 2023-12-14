// nll contrib for top when pingType == 'sbi'
	PARAMETER(logSigma_bi);		// Sigma for burst interval
	Type sigma_bi = exp(logSigma_bi);

	// nll -= dnorm(top(0),Type(0.0),Type(4.0),true);
	// nll -= dnorm(top(1),Type(approxBI),Type(4.0),true);
	// for(int i = 2; i < np; ++i)	{
		// nll -= dnorm(top(i)-2*top(i-1)+top(i-2), Type(0), sigma_bi, true);
		// // nll += bi_penalty * (softplus(0 - (top(i) - top(i-1)), bi_epsilon));
	// }


	nll -= dnorm(TOP(0), Type(0), Type(0.1), true);
	nll -= dnorm(TOP(0) - (-1 * TOP(2) + 2*TOP(1)), Type(0), sigma_bi, true);
	nll -= dnorm(TOP(1) - (-1 * TOP(3) + 2*TOP(2)), Type(0), sigma_bi, true);
	for(int i=2; i < np; i++){
	    nll -= dnorm(TOP(i)-2*TOP(i-1)+TOP(i-2), Type(0), sigma_bi, true);
	}
