{
	"patcher" : 	{
		"fileversion" : 1,
		"appversion" : 		{
			"major" : 9,
			"minor" : 0,
			"revision" : 2,
			"architecture" : "x64",
			"modernui" : 1
		}
,
		"classnamespace" : "box",
		"rect" : [ 794.0, 108.0, 676.0, 833.0 ],
		"gridsize" : [ 15.0, 15.0 ],
		"boxes" : [ 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-23",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 512.0, 152.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-19",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 502.0, 187.0, 116.0, 22.0 ],
					"text" : "/viable_threshold $1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-17",
					"maxclass" : "number",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 512.0, 244.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-14",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 507.0, 275.0, 114.0, 22.0 ],
					"text" : "/speed_of_comp $1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-9",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 292.5, 133.0, 79.0, 22.0 ],
					"text" : "/write_history"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-13",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 389.0, 270.0, 77.0, 22.0 ],
					"text" : "/brownian $1"
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-10",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 389.0, 239.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-38",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 307.0, 275.0, 79.0, 22.0 ],
					"text" : "/max_tour $1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-35",
					"maxclass" : "number",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 307.0, 234.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-30",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 187.0, 275.0, 116.0, 22.0 ],
					"text" : "/exploration_bias $1"
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-18",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 187.0, 239.0, 73.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-15",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 197.0, 25.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-11",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 197.0, 53.0, 146.0, 22.0 ],
					"text" : "expr 1. - (1.0 / ($f1 + 1.0))"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-2",
					"maxclass" : "preset",
					"numinlets" : 1,
					"numoutlets" : 5,
					"outlettype" : [ "preset", "int", "preset", "int", "" ],
					"patching_rect" : [ 14.0, 16.0, 100.0, 40.0 ],
					"preset_data" : [ 						{
							"number" : 1,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.5, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.001000000047497, 5, "obj-35", "number", "int", 6, 5, "obj-10", "number", "float", 0.0, 5, "obj-17", "number", "int", 50, 5, "obj-23", "number", "float", 0.0 ]
						}
, 						{
							"number" : 2,
							"data" : [ 5, "obj-21", "number", "float", 1.309999942779541, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.389999985694885, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.000159999995958, 5, "obj-35", "number", "int", 4, 5, "obj-10", "number", "float", 0.0, 5, "obj-17", "number", "int", 40, 5, "obj-23", "number", "float", 0.0 ]
						}
, 						{
							"number" : 3,
							"data" : [ 5, "obj-21", "number", "float", 1.200000047683716, 5, "obj-24", "number", "float", 2.0, 5, "obj-29", "number", "float", 0.409999996423721, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.000009999999747, 5, "obj-35", "number", "int", 49, 5, "obj-10", "number", "float", 0.0, 5, "obj-17", "number", "int", 12, 5, "obj-23", "number", "float", 0.0 ]
						}
, 						{
							"number" : 4,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 2.0, 5, "obj-29", "number", "float", 0.449999988079071, 5, "obj-31", "number", "float", 3.0, 5, "obj-36", "number", "int", 0, 5, "obj-15", "number", "float", 0.0, 5, "obj-18", "number", "float", 0.070000000298023, 5, "obj-35", "number", "int", 6 ]
						}
, 						{
							"number" : 6,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.282000005245209, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.000099999997474, 5, "obj-35", "number", "int", 49, 5, "obj-10", "number", "float", 0.0 ]
						}
, 						{
							"number" : 9,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.389999985694885, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.0, 5, "obj-35", "number", "int", 4, 5, "obj-10", "number", "float", 0.009999999776483 ]
						}
, 						{
							"number" : 17,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.5, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.000099999997474, 5, "obj-35", "number", "int", 6, 5, "obj-10", "number", "float", 0.002000000094995, 5, "obj-17", "number", "int", 20, 5, "obj-23", "number", "float", 4.456999778747559 ]
						}
, 						{
							"number" : 20,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.5, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.00109999999404, 5, "obj-35", "number", "int", 14, 5, "obj-10", "number", "float", 0.0, 5, "obj-17", "number", "int", 5, 5, "obj-23", "number", "float", 61.0 ]
						}
, 						{
							"number" : 21,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.5, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.001000000047497, 5, "obj-35", "number", "int", 14, 5, "obj-10", "number", "float", 0.0, 5, "obj-17", "number", "int", 1, 5, "obj-23", "number", "float", 0.170000001788139 ]
						}
, 						{
							"number" : 23,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.5, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.001000000047497, 5, "obj-35", "number", "int", 4, 5, "obj-10", "number", "float", 0.0, 5, "obj-17", "number", "int", 15, 5, "obj-23", "number", "float", 100.0 ]
						}
, 						{
							"number" : 24,
							"data" : [ 5, "obj-21", "number", "float", 1.0, 5, "obj-24", "number", "float", 1.0, 5, "obj-29", "number", "float", 0.439999997615814, 5, "obj-31", "number", "float", 1.0, 5, "obj-36", "number", "int", 10, 5, "obj-15", "number", "float", 1.0, 5, "obj-18", "number", "float", 0.001000000047497, 5, "obj-35", "number", "int", 49, 5, "obj-10", "number", "float", 0.0, 5, "obj-17", "number", "int", 1 ]
						}
 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-7",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 64.5, 75.0, 79.0, 20.0 ],
					"text" : "default = 100"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-6",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 146.5, 75.0, 100.0, 20.0 ],
					"text" : "1 = evaporate all"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-5",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 102.0, 214.0, 79.0, 20.0 ],
					"text" : "Distance"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-4",
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 19.0, 214.0, 79.0, 20.0 ],
					"text" : "Pheromones"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-36",
					"maxclass" : "number",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 18.0, 100.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-34",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 18.0, 138.0, 81.0, 22.0 ],
					"text" : "/num_ants $1"
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-31",
					"maxclass" : "flonum",
					"minimum" : 0.0,
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 79.0, 100.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-29",
					"maxclass" : "flonum",
					"maximum" : 1.0,
					"minimum" : 0.0,
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 167.0, 100.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-27",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 98.0, 133.0, 67.0, 22.0 ],
					"text" : "/deposit $1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-26",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 161.0, 133.0, 91.0, 22.0 ],
					"text" : "/evaporation $1"
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-24",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 102.0, 245.0, 68.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-22",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 100.0, 277.0, 52.0, 22.0 ],
					"text" : "/beta $1"
				}

			}
, 			{
				"box" : 				{
					"format" : 6,
					"id" : "obj-21",
					"maxclass" : "flonum",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "", "bang" ],
					"parameter_enable" : 0,
					"patching_rect" : [ 24.0, 245.0, 50.0, 22.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-3",
					"maxclass" : "message",
					"numinlets" : 2,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 24.0, 272.0, 58.0, 22.0 ],
					"text" : "/alpha $1"
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-1",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 18.0, 171.0, 142.0, 22.0 ],
					"text" : "udpsend localhost 57666"
				}

			}
 ],
		"lines" : [ 			{
				"patchline" : 				{
					"destination" : [ "obj-13", 0 ],
					"midpoints" : [ 398.5, 264.0, 398.5, 264.0 ],
					"source" : [ "obj-10", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-29", 0 ],
					"midpoints" : [ 206.5, 96.0, 176.5, 96.0 ],
					"source" : [ "obj-11", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 398.5, 309.0, 3.0, 309.0, 3.0, 168.0, 27.5, 168.0 ],
					"source" : [ "obj-13", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 516.5, 311.9921875, 3.0, 311.9921875, 3.0, 168.0, 27.5, 168.0 ],
					"source" : [ "obj-14", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-11", 0 ],
					"midpoints" : [ 206.5, 48.0, 206.5, 48.0 ],
					"source" : [ "obj-15", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-14", 0 ],
					"source" : [ "obj-17", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-30", 0 ],
					"midpoints" : [ 196.5, 264.0, 196.5, 264.0 ],
					"source" : [ "obj-18", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 511.5, 219.0, 269.5, 219.0, 269.5, 161.0, 27.5, 161.0 ],
					"source" : [ "obj-19", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"midpoints" : [ 33.5, 270.0, 33.5, 270.0 ],
					"source" : [ "obj-21", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 109.5, 309.0, 3.0, 309.0, 3.0, 168.0, 27.5, 168.0 ],
					"source" : [ "obj-22", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-19", 0 ],
					"source" : [ "obj-23", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-22", 0 ],
					"midpoints" : [ 111.5, 270.0, 109.5, 270.0 ],
					"source" : [ "obj-24", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 170.5, 156.0, 99.0, 156.0, 99.0, 165.0, 27.5, 165.0 ],
					"source" : [ "obj-26", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 107.5, 156.0, 99.0, 156.0, 99.0, 165.0, 27.5, 165.0 ],
					"source" : [ "obj-27", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-26", 0 ],
					"midpoints" : [ 176.5, 123.0, 170.5, 123.0 ],
					"source" : [ "obj-29", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 33.5, 297.0, 3.0, 297.0, 3.0, 168.0, 27.5, 168.0 ],
					"source" : [ "obj-3", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 196.5, 309.0, 3.0, 309.0, 3.0, 168.0, 27.5, 168.0 ],
					"source" : [ "obj-30", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-27", 0 ],
					"midpoints" : [ 88.5, 129.0, 107.5, 129.0 ],
					"source" : [ "obj-31", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 27.5, 162.0, 27.5, 162.0 ],
					"source" : [ "obj-34", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-38", 0 ],
					"midpoints" : [ 316.5, 258.0, 316.5, 258.0 ],
					"source" : [ "obj-35", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-34", 0 ],
					"midpoints" : [ 27.5, 123.0, 27.5, 123.0 ],
					"source" : [ "obj-36", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 316.5, 309.0, 3.0, 309.0, 3.0, 168.0, 27.5, 168.0 ],
					"source" : [ "obj-38", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-1", 0 ],
					"midpoints" : [ 302.0, 168.0, 27.5, 168.0 ],
					"source" : [ "obj-9", 0 ]
				}

			}
 ],
		"originid" : "pat-12",
		"dependency_cache" : [  ],
		"autosave" : 0
	}

}
