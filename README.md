项目目标
--------
ai_npm的目标是实现一个私有的npm仓库，让软件研发企业可以自行搭建一个npm仓库，用来管理自己的npm包。

该项目和ai无任何关系，只是一个简单的npm仓库管理软件

项目功能
---------
[x] 上游（registry.npmjs.org）的镜像     
[x] npm包私有发布    
[ ] 私有npm包获取      
[ ] 开源npm包私有覆盖   
[ ] 开源npm包版本和名字锁定（防逆发布，删包等情况）   
[ ] npm别名机制     
[ ] 用户管理和认证  
[ ] web管理界面     
[ ] npm包搜索

同类项目
--------
1. cnpmjs 
2. nexus

同类项目差异
-----------
1. 与cnpmjs相比，无需安装cnpm，直接使用npm-cli来完成所有操作
2. 与nexus相比，支持npm别名支持和开源npm覆盖

使用发法
------------

1.修改host文件或内网DNS将npm.local或自定义域名指向本项目所在机器的ip地址

2.修改npm的 registry 

    npm set registry http://npm.local:4873/