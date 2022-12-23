# 1
# r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility
read_csv("nonnegative.csv") %>% inner_join(read_csv("r_d.csv")) %>% inner_join(read_csv("major_customer.csv")) %>% 
inner_join(read_csv("bm.csv")) %>% inner_join(read_csv("e_index.csv")) %>% inner_join(read_csv("female_ceo.csv")) %>% 
inner_join(read_csv("growth.csv")) %>% inner_join(read_csv("leverage.csv")) %>% inner_join(read_csv("pay_turn.csv")) %>% 
inner_join(read_csv("roa.csv")) %>% inner_join(read_csv("size.csv")) %>% inner_join(read_csv("tangibility.csv")) %>% 
separate(gvkey_fyear, c("gvkey", "fyear")) %>% mutate(r_d = Winsorize(r_d, probs = c(0.01, 0.99))) %>% 
mutate(bm = Winsorize(bm, probs = c(0.01, 0.99))) %>% mutate(growth = Winsorize(growth, probs = c(0.01, 0.99))) %>% 
mutate(leverage = Winsorize(leverage, probs = c(0.01, 0.99))) %>% mutate(pay_turn = Winsorize(pay_turn, probs = c(0.01, 0.99))) %>% 
mutate(roa = Winsorize(roa, probs = c(0.01, 0.99))) %>% mutate(size = Winsorize(size, probs = c(0.01, 0.99))) %>% 
mutate(tangibility = Winsorize(tangibility, probs = c(0.01, 0.99))) %>% write_csv("sample_1.csv")

# 2
# r_d ~ customer_hhi + bm + e_index + eqinc + female_ceo + growth + leverage + pay_turn + roa + size + tangibility
read_csv("r_d.csv") %>% inner_join(read_csv("customer_hhi.csv")) %>% inner_join(read_csv("bm.csv")) %>% inner_join(read_csv("e_index.csv")) %>% inner_join(read_csv("eqinc.csv")) %>% inner_join(read_csv("female_ceo.csv")) %>% inner_join(read_csv("growth.csv")) %>% inner_join(read_csv("leverage.csv")) %>% inner_join(read_csv("pay_turn.csv")) %>% inner_join(read_csv("roa.csv")) %>% inner_join(read_csv("size.csv")) %>% inner_join(read_csv("tangibility.csv")) %>% separate(col = gvkey_fyear, into = c("gvkey", "fyear"), sep = "_") %>% write_csv("data_2.csv")


