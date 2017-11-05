module CompilerConstants where

type BinOp = (String -> String -> String -> (String, String))

data CompilerStrings = CompilerStrings {
    header :: String, 
    expHeader :: String, 
    newVariable :: String -> String,
    newExpLoc :: Int -> String,
    store :: String -> String -> (String, String),
    load :: String -> String -> (String, String),
    addE :: BinOp,
    subE :: BinOp,
    mulE :: BinOp,
    divE :: BinOp,
    lit :: String -> (String, String),
    expFooter :: String -> String,
    footer :: String
}

const3 = flip $ const $ flip (const const)

jvmStrings = CompilerStrings {
    header=".class  public Hello\n.super  java/lang/Object\n\n.method public <init>()V\naload_0\ninvokespecial java/lang/Object/<init>()V\nreturn\n.end method\n\n.method public static main([Ljava/lang/String;)V\n.limit stack 1000\n.limit locals 1000\n",
    expHeader="getstatic java/lang/System/out Ljava/io/PrintStream;\n",
    newVariable=const "",
    newExpLoc=const "",
    store=(\var_name -> const ("istore " ++ var_name ++ "\n", "")),
    load=(\var_name -> const ("iload " ++ var_name ++ "\n", "")),
    addE=const3 ("iadd\n", ""),
    subE=const3 ("isub\n", ""),
    mulE=const3 ("imul\n", ""),
    divE=const3 ("idiv\n", ""),
    lit=(\number -> ("bipush " ++ number ++ "\n", "")),
    expFooter=const "invokevirtual java/io/PrintStream/println(I)V\n",
    footer="return\n.end method\n"
}

llvmBinOp :: String -> BinOp
llvmBinOp op loc1 loc2 loc3 = (loc3 ++ " = " ++ op ++ " i32 " ++ loc1 ++ ", " ++ loc2 ++ "\n", loc3)

llvmStrings = CompilerStrings {
    header="@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n" ++
            "declare i32 @printf(i8*, ...)\n" ++
            "define void @printInt(i32 %x) {\n" ++
            "entry: %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n" ++
                "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n" ++
                "ret void\n" ++
            "}\n\n" ++
            "define i32 @main() {\n",
    expHeader="",
    newVariable=(\var_id -> "%var_" ++ var_id ++ " = alloca i32\n"),
    newExpLoc=(\exp_id -> "%tmp_" ++ (show exp_id)),
    store=(\var_name -> \loc -> ("store i32 " ++ loc ++ ", i32* %var_" ++ var_name ++ "\n", "")),
    load=(\var_name -> \loc -> (loc ++ " = load i32, i32* %var_" ++ var_name ++ "\n", loc)),
    addE=llvmBinOp "add",
    subE=llvmBinOp "sub",
    mulE=llvmBinOp "mul",
    divE=llvmBinOp "sdiv",
    lit=(\number -> ("", number)),
    expFooter=(\loc -> "call void @printInt(i32 " ++ loc ++ ")\n"),
    footer="ret i32 0}\n"

} 
    
    
