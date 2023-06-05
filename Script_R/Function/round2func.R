round2 = function(x, n) {
    posneg = sign(x)       # 양수/음수 확인 (양수 = 1, 음수 = -1)
    z = abs(x)*10^n        # abs : 절대값 (아래 z + 0.5 때문에 절대값 필요)
    z = trunc(z + 0.5)     # 소수점 아래 버림 
    z = z/10^n
    z = z*posneg           # 계산결과에 원래 양수/음수로 전환
    return(z)
}