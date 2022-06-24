X11(type="Xlib")

library(beepr)  # 导入发声包

# 初始化环境变量
init<-function(){
  e<<-new.env()
  e$stage<-0 #场景
  e$width<-e$height<-20  #切分格子
  e$step<-1/e$width #步长
  e$m<-matrix(rep(0,e$width*e$height),nrow=e$width)  #点矩阵
  e$dir<-e$lastd<-'up' # 移动方向
  e$head<-c(2,2) #初始蛇头
  e$lastx<-e$lasty<-2 # 初始化蛇头上一个点
  e$tail<-data.frame(x=c(),y=c())#初始蛇尾
  
  e$col_furit<-2 #水果颜色
  e$col_head<-4 #蛇头颜色
  e$col_tail<-8 #蛇尾颜色
  e$col_path<-0 #路颜色

#增加的初始化变量
  e$eat_num<-0 #连续吃果子次数
  e$block<-c(1,10)  #初始障碍物
  e$bdir<-"right" #障碍物移动方向
  e$col_block<-9 #障碍物颜色
  #e$if_fruit<-1
}


# 获得矩阵的索引值
index<-function(col) which(e$m==col)

# 游戏中
stage1<-function(){
  e$stage<-1
  
  # 随机的水果点
  furit<-function(){
    if(length(index(e$col_furit))<=0){ #不存在水果
      idx<-sample(index(e$col_path),1)
      
      fx<-ifelse(idx%%e$width==0,10,idx%%e$width)
      fy<-ceiling(idx/e$height)
      e$m[fx,fy]<-e$col_furit
      
      print(paste("furit idx",idx))
      print(paste("furit axis:",fx,fy))
    }
  }
  
  
  # 检查失败
  fail<-function(){
    # head出边界
    if(e$head[1] == 0) {  # 判断蛇头是否出去
      e$head[1] = 19  # 会到另一个方向
    } else if (e$head[1] == 21) {
      e$head[1] = 1
    } 
    if(e$head[2] == 0) {  # 判断蛇头是否出去
      e$head[2] = 19
    } else if (e$head[2] == 21) {
      e$head[2] = 1
    } 
    
    # head碰到tail
    if(e$m[e$head[1],e$head[2]]==e$col_tail){
      print("game over: head hit tail")
      keydown('q')
      beep(3)
      return(TRUE)
    }
    
    
    # # tail碰到block
    # if(e$m[e$block[1],e$block[2]]==e$col_tail){
    #   print("game over: tail hit block.")
    #   keydown('q')
    #   beep(3)
    #   return(TRUE)
    # }
    # 
    # # head碰到block
    # if(length(index(e$col_head))<=0){
    #   print("game over: head hit block")
    #   keydown('q')
    #   beep(3)
    #   return(TRUE)
    # }
    
    return(FALSE)
  }
  
  
  # snake head
  head<-function(){
    e$lastx<-e$head[1]
    e$lasty<-e$head[2]
    
    # 方向操作
    if(e$dir=='up') e$head[2]<-e$head[2]+1
    if(e$dir=='down') e$head[2]<-e$head[2]-1
    if(e$dir=='left') e$head[1]<-e$head[1]-1
    if(e$dir=='right') e$head[1]<-e$head[1]+1
    
  }
  
  # snake body
  body<-function(){
    
    e$m[e$lastx,e$lasty]<-0
    e$m[e$head[1],e$head[2]]<-e$col_head #snake
    
    #新加代码
    e$m[e$lastbx,e$lastby]<-0
    e$m[e$block[1],e$block[2]]<-e$col_block
    
    print(data.frame(x=e$lastx,y=e$lasty))
    if(e$eat_num == 3){# 连续吃三次果子
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))  # 矩阵合并,增加节数
      e$eat_num<-0  # 重置吃果子数
    }
    if(length(index(e$col_furit))<=0){ #不存在水果
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))  # 矩阵合并
      beep(2)  # 播放声音
      e$eat_num<-e$eat_num+1
    }
    
    if(nrow(e$tail)>0) { #如果有尾巴
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
      e$m[e$tail[1,]$x,e$tail[1,]$y]<-e$col_path
      e$tail<-e$tail[-1,]
      e$m[e$lastx,e$lasty]<-e$col_tail
    }
    
    print(paste("snake idx",index(e$col_head)))
    print(paste("snake axis:",e$head[1],e$head[2]))
  }
  
  # 画布背景
  drawTable<-function(){
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  }
  
  # 根据矩阵画数据
  drawMatrix<-function(){
    idx<-which(e$m>0)
    px<- (ifelse(idx%%e$width==0,e$width,idx%%e$width)-1)/e$width+e$step/2
    py<- (ceiling(idx/e$height)-1)/e$height+e$step/2
    pxy<-data.frame(x=px,y=py,col=e$m[idx])
    points(pxy$x,pxy$y,col=pxy$col,pch=15,cex=4.4)
    
    #增加的文本款
    text(0.5,0.95,label=paste("You have eat",nrow(e$tail),"fruits!"),cex=2,col=2)  # 添加得分
  }
  
  furit()
  head()
  
  if(!fail()){
    body()
    drawTable()
    drawMatrix()  
  }
}


# 开机画图
stage0<-function(){
  e$stage<-0
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="Snake Game",cex=5)
  text(0.5,0.4,label="Any keyboard to start",cex=2,col=4)
  text(0.5,0.3,label="Up,Down,Left,Rigth to control direction",cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
}

# 结束画图
stage2<-function(){
  e$stage<-2
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="Game Over",cex=5)
  text(0.5,0.4,label="Space to restart, q to quit.",cex=2,col=4)
  text(0.5,0.3,label=paste("Congratulations! You have eat",nrow(e$tail),"fruits!"),cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
}

# 增加暂停画图
stage3<-function() {
  e$stage<-3
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  text(0.5,0.7,label="游戏已暂停",cex=5)
  text(0.5,0.4,label="按下q退出，按下p继续",cex=2,col=4)
  text(0.5,0.3,label=paste("Congratulations! You have eat",nrow(e$tail),"fruits!"),cex=2,col=2)
  text(0.2,0.05,label="Author:DanZhang",cex=1)
  text(0.5,0.05,label="http://blog.fens.me",cex=1)
}

# 键盘事件
keydown<-function(K){
  print(paste("keydown:",K,",stage:",e$stage));
  
  if(e$stage==3) {# 在暂停画面中
    if(K == "q"){
      stage2()  # 如果按下q键就会结束
    } 
    if(K == "p"){
      stage1()  # 如果按下p，则会继续进行游戏
    }
    return(NULL)
  }
  
  if(e$stage==0){ # 开机画面
    init()
    stage1()
    return(NULL)
  }  
  
  if(e$stage==2){ # 结束画面
    if(K=="q") q()  
    else if(K==' ') stage0()  
    return(NULL)
  } 
  
  if(e$stage==1){ # 在游戏进行中
    if(K == "q") {
      stage2() # 如果按下q键就会结束
    } 
    if (K == "p") {
      stage3()  # 如果按下p，则进入暂停画面
    } else {
      if(tolower(K) %in% c("up","down","left","right")){
        e$lastd<-e$dir
        e$dir<-tolower(K)
        stage1()  
      }
    }
  }
  return(NULL)
}


run<-function(){
  par(mai=rep(0,4),oma=rep(0,4))
  e<<-new.env()
  stage0()
  
  # 注册事件
  getGraphicsEvent(prompt="Snake Game",onKeybd=keydown)
}

run()