ghci> validate testDatum testRedeemer (testContext [alice, bob] 1700000001)
True
ghci> validate testDatum testRedeemer (testContext [alice, bob] 1699999999)
False
ghci> validate testDatum (Spend [alice]) (testContext [alice] 1700000001)
False
ghci> let mallory = "mallory_pkh"
ghci> validate testDatum (Spend [alice, mallory]) (testContext [alice, mallory] 1700000001)
False
