// nll contrib for top when pingType == 'rbi'
	nll -= dnorm(top(0),Type(0.0),Type(4.0),true);
	for(int i = 1; i < np; ++i)	{
		nll -= dnorm(top(i), top(i-1) + (rbi_min + (rbi_max - rbi_min)/2), (rbi_max-rbi_min)/2, true);
		nll += bi_penalty * (softplus((top(i) - top(i-1)) - rbi_max, bi_epsilon) + softplus(rbi_min - (top(i) - top(i-1)), bi_epsilon));
	}
