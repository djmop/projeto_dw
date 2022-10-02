# box::use(G.LOG = ./box/globals/Z2_global_logging)
box::reload(G.LOG)

Logger <- G.LOG$Logger
logger <- Logger$new(FALSE)

logger$start()
logger$close()
logger$error_msg(caller = 'log("A")', problem = 'Error msg')

