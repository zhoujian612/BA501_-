pname = c('Gizmo','Powergizmo','SingleTouch',
          'MultiTouch','DoubleTouch')
price = c(19.99,29.99,149.99,203.99, 19.99)
category = c('Gadgets','Gadgets','Photography',
             'Household','Photography')
manufacturer = c('GizmoWorks','GizmoWorks','Canon',
                 'Hitachi','Canon')
product = data.frame(PName = pname, Price = price,
                     Category = category, 
                     Manufacturer = manufacturer)
product

library(sqldf)
#### basic
# 1, get product name, price, category name, manufacturer
# for products price > 30, display by price high to low

# 2, get product name (PName) and price 
# for products name that contains 'touch'

# 3, get category name whose average price > $30

# 4, get category name for the most expensive product

#### intermediate
flight = data.frame(departure = c('la','la','ny','ny','sf','sf'),
                    arrival = c('ny','sf','la','sf','aus','ny'))
# 给一个flight的table，有depature city和 arrival city，求unique的不论顺序的 组合。比如 depature, arrival
# A      B
# B      A
# 结果只出现 A B。

#### 三个ins table.

t1 = data.frame(user_name = c('LBJ','Curry','Brady'), 
                sports_category = c('basketball','basketball','football'))

t2 = data.frame(user_id = c(11,22,33,44,55,66,77,88,99),
                user_name = c('LBJ','Curry','Brady','fan4','fan5','fan6',
                              'fan7','fan8','fan9'))
t3 = data.frame(user_id = c(11,11,22,22,22,33,33,33,33),
                user_id_following = c(22,44,11,55,66,77,88,99,44))

# t1(user_name, sports_category) 
# t1中只有celebrity运动员。pk=user_name
# t2(user_id, user_name, registration_date)
# t2中是所有人的用户信息，包括celebrity和普通人，
# 且不会出现celebrity和普通人重名的情况（重要假设）。pk=user_id
# t3(user_id, user_id_following, follow_date)
# 用户follow信息，注意user_id_following中包括celebrity和普通人
# 计算每个category有多少人follow
### 
member = data.frame(member_id = c(11,11,22,22,33,44),
                    email = c('x11_1@gmail', 'x11_2@gmail',
                              'x22_1@gmail','x22_2@gmail',
                              'x33@gmail','x44@gmail'))
# member_id, email_address
# 一个member 可能有2个email address
# output：memeber id，email1，email2