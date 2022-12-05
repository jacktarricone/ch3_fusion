library(sen2r)
sen2r()
sen2r("/Users/jacktarricone/.sen2r/proc_par/s2proc_20221204_190311.json")
# s2_order("/Users/jacktarricone/.sen2r/lta_orders/lta_20221118_121440.json")
# s2_order("/Users/jacktarricone/.sen2r/lta_orders/lta_20221122_083828.json", service = "dhus")
# safe_is_online("/Users/jacktarricone/.sen2r/lta_orders/lta_20221122_083828.json")

safe_is_online("/Users/jacktarricone/.sen2r/lta_orders/lta_20221130_084330.json")
s2_download("/Users/jacktarricone/.sen2r/lta_orders/lta_20221130_084330.json", service = "dhus")
s2_order("/Users/jacktarricone/ch3_fusion/scripts/images_for_poster.json", service = "dhus")

