@dnl = internal constant [4 x i8] c"%d\0A\00"
@d   = internal constant [3 x i8] c"%d\00"
@.str = internal constant [6 x i8] c"%[^\0A]\00"
@.err = internal constant [14 x i8] c"runtime error\00"

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare i8* @malloc(i32)
declare i32 @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare void @exit(i32)

define void @printInt(i32 %x) {
entry: %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0
	call i32 (i8*, ...)* @printf(i8* %t0, i32 %x) 
	ret void
}

define i32 @readInt() {
entry:	%res = alloca i32
        %t1 = getelementptr [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...)* @scanf(i8* %t1, i32* %res)
	%t2 = load i32* %res
	ret i32 %t2
}

define void @printString(i8* %s) {
entry:  call i32 @puts(i8* %s)
	ret void
}

define i8* @readString() {
  %buff = alloca i8*
  %1 = call i8* @malloc(i32 100)
  store i8* %1, i8** %buff
  %2 = load i8** %buff
  %3 = call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([6 x i8]* @.str, i32 0, i32 0), i8* %2)
  %4 = load i8** %buff
  ret i8* %4
}

define i8* @_concat(i8* %s1, i8* %s2) {
    %1 = call i32 @strlen(i8* %s1)
    %2 = call i32 @strlen(i8* %s2)
    %3 = add i32 %1, 1
    %4 = add i32 %3, %2
    %5 = call i8* @malloc(i32 %4)
    %6 = call i8* @strcpy(i8* %5, i8* %s1)
    %7 = call i8* @strcat(i8* %6, i8* %s2)
    ret i8* %7
}

define void @error() {
    %1 = bitcast [14 x i8]* @.err to i8*
    call void @printString(i8* %1)
    call void @exit(i32 1)
    ret void
}
