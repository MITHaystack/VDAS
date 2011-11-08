{application,mark6,
	[
	{description, "Mark6 control plane"},
	{vsn, "0.1"},
	{modules,[mark6_app,mark6_sup,m6_state,x3c_server,m6_x3c,m6_server,m6_vsis]},
	{registered,[m6_parser]},
	{applications,[kernel,stdlib]},
	{env, [
		  {listen_port, 6050},
		  {block_size, 1024}
		  ]
	},
	{mod,{mark6_app,[]}}
	]
}.
