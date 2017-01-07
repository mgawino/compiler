%struct.ArrInt = type {i32, i32*}
%struct.ArrStr = type {i32, i8**}
%struct.ArrBool = type {i32, i1*}
%struct.ArrObj = type {i32, i8**}

declare void @printInt (i32)
declare void @printString (i8*)
declare void @error ()
declare i32 @readInt ()
declare i8* @readString ()
declare i8* @_concat (i8*, i8*)
declare i8* @malloc (i32)
define %struct.ArrInt* @doubleArray (%struct.ArrInt* %a)
{
_L0:
%_1 = alloca %struct.ArrInt*
store %struct.ArrInt* %a , %struct.ArrInt** %_1
%_2 = alloca %struct.ArrInt*
%_3 = call i8* @malloc(i32 12)
%_4 = bitcast i8* %_3 to %struct.ArrInt*
%_5 = getelementptr %struct.ArrInt* %_4 , i32 0, i32 0
%_6 = load %struct.ArrInt** %_1
%_7 = getelementptr %struct.ArrInt* %_6 , i32 0, i32 0
%_8 = load i32* %_7
%_9 = mul i32 4 , %_8
store i32 %_9 , i32* %_5
%_10 = getelementptr %struct.ArrInt* %_4 , i32 0, i32 1
%_11 = call i8* @malloc(i32 %_9)
%_12 = bitcast i8* %_11 to i32*
store i32* %_12 , i32** %_10
store %struct.ArrInt* %_4 , %struct.ArrInt** %_2
%_13 = alloca i32
store i32 0 , i32* %_13
%_14 = alloca i32
store i32 0 , i32* %_14
%_15 = alloca i32
store i32 0 , i32* %_15
br label %_L16
_L16:
%_19 = load i32* %_14
%_20 = load %struct.ArrInt** %_1
%_21 = getelementptr %struct.ArrInt* %_20 , i32 0, i32 0
%_22 = load i32* %_21
%_23 = icmp slt i32 %_19 , %_22
br i1 %_23 , label %_L17 , label %_L18
_L17:
%_24 = load %struct.ArrInt** %_1
%_25 = load i32* %_14
%_26 = getelementptr %struct.ArrInt* %_24 , i32 0, i32 1
%_27 = load i32** %_26
%_28 = getelementptr i32* %_27 , i32 %_25
%_29 = load i32* %_28
store i32 %_29 , i32* %_15
%_30 = load %struct.ArrInt** %_2
%_31 = load i32* %_13
%_32 = load i32* %_15
%_33 = mul i32 2 , %_32
%_34 = getelementptr %struct.ArrInt* %_30 , i32 0, i32 1
%_35 = load i32** %_34
%_36 = getelementptr i32* %_35 , i32 %_31
store i32 %_33 , i32* %_36
%_37 = load i32* %_13
%_38 = add i32 %_37 , 1
store i32 %_38 , i32* %_13
%_39 = load i32* %_14
%_40 = add i32 %_39 , 1
store i32 %_40 , i32* %_14
br label %_L16
_L18:
%_41 = load %struct.ArrInt** %_2
ret %struct.ArrInt* %_41
}
define void @shiftLeft (%struct.ArrInt* %a)
{
_L42:
%_43 = alloca %struct.ArrInt*
store %struct.ArrInt* %a , %struct.ArrInt** %_43
%_44 = alloca i32
%_45 = load %struct.ArrInt** %_43
%_46 = getelementptr %struct.ArrInt* %_45 , i32 0, i32 1
%_47 = load i32** %_46
%_48 = getelementptr i32* %_47 , i32 0
%_49 = load i32* %_48
store i32 %_49 , i32* %_44
%_50 = alloca i32
store i32 0 , i32* %_50
br label %_L51
_L51:
%_54 = load i32* %_50
%_55 = load %struct.ArrInt** %_43
%_56 = getelementptr %struct.ArrInt* %_55 , i32 0, i32 0
%_57 = load i32* %_56
%_58 = sub i32 %_57 , 1
%_59 = icmp slt i32 %_54 , %_58
br i1 %_59 , label %_L52 , label %_L53
_L52:
%_60 = load %struct.ArrInt** %_43
%_61 = load i32* %_50
%_62 = load %struct.ArrInt** %_43
%_63 = load i32* %_50
%_64 = add i32 %_63 , 1
%_65 = getelementptr %struct.ArrInt* %_62 , i32 0, i32 1
%_66 = load i32** %_65
%_67 = getelementptr i32* %_66 , i32 %_64
%_68 = load i32* %_67
%_69 = getelementptr %struct.ArrInt* %_60 , i32 0, i32 1
%_70 = load i32** %_69
%_71 = getelementptr i32* %_70 , i32 %_61
store i32 %_68 , i32* %_71
%_72 = load i32* %_50
%_73 = add i32 %_72 , 1
store i32 %_73 , i32* %_50
br label %_L51
_L53:
%_74 = load %struct.ArrInt** %_43
%_75 = load %struct.ArrInt** %_43
%_76 = getelementptr %struct.ArrInt* %_75 , i32 0, i32 0
%_77 = load i32* %_76
%_78 = sub i32 %_77 , 1
%_79 = load i32* %_44
%_80 = getelementptr %struct.ArrInt* %_74 , i32 0, i32 1
%_81 = load i32** %_80
%_82 = getelementptr i32* %_81 , i32 %_78
store i32 %_79 , i32* %_82
ret void
}
define i32 @scalProd (%struct.ArrInt* %a, %struct.ArrInt* %b)
{
_L83:
%_84 = alloca %struct.ArrInt*
store %struct.ArrInt* %a , %struct.ArrInt** %_84
%_85 = alloca %struct.ArrInt*
store %struct.ArrInt* %b , %struct.ArrInt** %_85
%_86 = alloca i32
store i32 0 , i32* %_86
%_87 = alloca i32
store i32 0 , i32* %_87
br label %_L88
_L88:
%_91 = load i32* %_87
%_92 = load %struct.ArrInt** %_84
%_93 = getelementptr %struct.ArrInt* %_92 , i32 0, i32 0
%_94 = load i32* %_93
%_95 = icmp slt i32 %_91 , %_94
br i1 %_95 , label %_L89 , label %_L90
_L89:
%_96 = load i32* %_86
%_97 = load %struct.ArrInt** %_84
%_98 = load i32* %_87
%_99 = getelementptr %struct.ArrInt* %_97 , i32 0, i32 1
%_100 = load i32** %_99
%_101 = getelementptr i32* %_100 , i32 %_98
%_102 = load i32* %_101
%_103 = load %struct.ArrInt** %_85
%_104 = load i32* %_87
%_105 = getelementptr %struct.ArrInt* %_103 , i32 0, i32 1
%_106 = load i32** %_105
%_107 = getelementptr i32* %_106 , i32 %_104
%_108 = load i32* %_107
%_109 = mul i32 %_102 , %_108
%_110 = add i32 %_96 , %_109
store i32 %_110 , i32* %_86
%_111 = load i32* %_87
%_112 = add i32 %_111 , 1
store i32 %_112 , i32* %_87
br label %_L88
_L90:
%_113 = load i32* %_86
ret i32 %_113
}
define i32 @main ()
{
_L114:
%_115 = alloca %struct.ArrInt*
%_116 = call i8* @malloc(i32 12)
%_117 = bitcast i8* %_116 to %struct.ArrInt*
%_118 = getelementptr %struct.ArrInt* %_117 , i32 0, i32 0
%_119 = mul i32 4 , 5
store i32 %_119 , i32* %_118
%_120 = getelementptr %struct.ArrInt* %_117 , i32 0, i32 1
%_121 = call i8* @malloc(i32 %_119)
%_122 = bitcast i8* %_121 to i32*
store i32* %_122 , i32** %_120
store %struct.ArrInt* %_117 , %struct.ArrInt** %_115
%_123 = alloca i32
store i32 0 , i32* %_123
br label %_L124
_L124:
%_127 = load i32* %_123
%_128 = load %struct.ArrInt** %_115
%_129 = getelementptr %struct.ArrInt* %_128 , i32 0, i32 0
%_130 = load i32* %_129
%_131 = icmp slt i32 %_127 , %_130
br i1 %_131 , label %_L125 , label %_L126
_L125:
%_132 = load %struct.ArrInt** %_115
%_133 = load i32* %_123
%_134 = load i32* %_123
%_135 = getelementptr %struct.ArrInt* %_132 , i32 0, i32 1
%_136 = load i32** %_135
%_137 = getelementptr i32* %_136 , i32 %_133
store i32 %_134 , i32* %_137
%_138 = load i32* %_123
%_139 = add i32 %_138 , 1
store i32 %_139 , i32* %_123
br label %_L124
_L126:
%_140 = load %struct.ArrInt** %_115
call void @shiftLeft (%struct.ArrInt* %_140)
%_142 = alloca %struct.ArrInt*
%_143 = load %struct.ArrInt** %_115
%_144 = call %struct.ArrInt* @doubleArray (%struct.ArrInt* %_143)
store %struct.ArrInt* %_144 , %struct.ArrInt** %_142
%_145 = alloca i32
store i32 0 , i32* %_145
%_146 = alloca i32
store i32 0 , i32* %_146
br label %_L147
_L147:
%_150 = load i32* %_145
%_151 = load %struct.ArrInt** %_115
%_152 = getelementptr %struct.ArrInt* %_151 , i32 0, i32 0
%_153 = load i32* %_152
%_154 = icmp slt i32 %_150 , %_153
br i1 %_154 , label %_L148 , label %_L149
_L148:
%_155 = load %struct.ArrInt** %_115
%_156 = load i32* %_145
%_157 = getelementptr %struct.ArrInt* %_155 , i32 0, i32 1
%_158 = load i32** %_157
%_159 = getelementptr i32* %_158 , i32 %_156
%_160 = load i32* %_159
store i32 %_160 , i32* %_146
%_161 = load i32* %_146
call void @printInt (i32 %_161)
%_163 = load i32* %_145
%_164 = add i32 %_163 , 1
store i32 %_164 , i32* %_145
br label %_L147
_L149:
%_165 = alloca i32
store i32 0 , i32* %_165
%_166 = alloca i32
store i32 0 , i32* %_166
br label %_L167
_L167:
%_170 = load i32* %_165
%_171 = load %struct.ArrInt** %_142
%_172 = getelementptr %struct.ArrInt* %_171 , i32 0, i32 0
%_173 = load i32* %_172
%_174 = icmp slt i32 %_170 , %_173
br i1 %_174 , label %_L168 , label %_L169
_L168:
%_175 = load %struct.ArrInt** %_142
%_176 = load i32* %_165
%_177 = getelementptr %struct.ArrInt* %_175 , i32 0, i32 1
%_178 = load i32** %_177
%_179 = getelementptr i32* %_178 , i32 %_176
%_180 = load i32* %_179
store i32 %_180 , i32* %_166
%_181 = load i32* %_166
call void @printInt (i32 %_181)
%_183 = load i32* %_165
%_184 = add i32 %_183 , 1
store i32 %_184 , i32* %_165
br label %_L167
_L169:
%_185 = load %struct.ArrInt** %_115
%_186 = load %struct.ArrInt** %_142
%_187 = call i32 @scalProd (%struct.ArrInt* %_185, %struct.ArrInt* %_186)
call void @printInt (i32 %_187)
ret i32 0
}
