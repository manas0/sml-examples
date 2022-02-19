exception NotUnsignedIntegers;
fun karatsuba (X, Y) =
	let

		fun vali ([])=true
			|vali (h::t)=
				if(ord(h)>=48 andalso ord(h)<=57) then
				vali t
				else false		

		fun reverse([])=[]
			|reverse (h::t)=(reverse t) @ [h]; 
		
		fun adjustZeros(A,m)=	
			let fun reverse []=[]
				|reverse (h::t)=(reverse t) @ [h];

				val P=reverse(explode(A))
	
				fun temp(P,m)=
				if m=0 then P
				else #"0"::temp(P,m-1)


			in 
			implode(reverse(temp(P,m)))
			end

		fun karatsuba1([],[])=[]
			|karatsuba1([h1],[h2])=[chr( ((ord(h1)-ord(#"0"))*(ord(h2)-ord(#"0")))div 10 + ord(#"0")),chr( ((ord(h1)-ord(#"0"))*(ord(h2)-ord(#"0")))mod 10 + ord(#"0"))]
			|karatsuba1(X as h1::t1,Y as h2::t2)=
				let
			
					fun split ([],l)=([],[])
						|split(h::t,l)=
							if l=0 then ([],h::t)
							else 
							let val (a,b)=split(t,l-1)
							in (h::a,b)
							end 

					fun add (M,N)=
						let fun add1 ([],[],carry)=[chr(carry+ord(#"0"))]
							|add1([],h::t,carry)=
								let val sum=(ord(h)-ord(#"0"))+carry;
								in
								if sum>=10 then
								chr(sum-10+ord(#"0"))::add1([],t,1)
								else
								chr(sum+ord(#"0"))::add1([],t,0)
								end

							|add1(h::t,[],carry)=
								let val sum=(ord(h)-ord(#"0"))+carry;
								in
								if sum>=10 then
								chr(sum-10+ord(#"0"))::add1(t,[],1)
								else
								chr(sum+ord(#"0"))::add1(t,[],0)
								end

	
							|add1(h1::t1,h2::t2,carry)=
								let val sum=(ord(h1)-ord(#"0")+ord(h2)-ord(#"0"))+carry
								in
								if sum>=10 then
								chr(sum-10+ord(#"0"))::add1(t1,t2,1)
								else
								chr(sum+ord(#"0"))::add1(t1,t2,0)
								end
		
					fun reverse []=[]
						|reverse (h::t)=(reverse t) @ [h]

					fun removeLeadingZeroes A=
						let fun aux []=[]
							|aux (h::t)=
								if ord(h)=ord(#"0") then aux t
								else
								h::t

							val X =explode(A)
						in
						implode(aux X)
						end 

					val A=reverse(explode(M))
					val B=reverse(explode(N))
					in
						removeLeadingZeroes(implode(reverse(add1(A,B,0))))
					end



			fun subtract (M,N)=
				let fun subtract1 ([],[],borrow)=[chr(ord(#"0")-borrow)]
					|subtract1(h::t,[],borrow)=
						let val dif=(ord(h)-ord(#"0"))-borrow;
						in
						if dif<0 then
						chr(dif+10+ord(#"0"))::subtract1(t,[],1)
						else
						chr(dif+ord(#"0"))::t
						end

	
					|subtract1(h1::t1,h2::t2,borrow)=
						let val dif=(ord(h1)-ord(h2))-borrow
						in
						if dif<0 then
						chr(dif+10+ord(#"0"))::subtract1(t1,t2,1)
						else
						chr(dif+ord(#"0"))::subtract1(t1,t2,0)
						end
				fun reverse []=[]
					|reverse (h::t)=(reverse t) @ [h]
	
				fun removeLeadingZeroes A=
					let fun aux []=[]
						|aux (h::t)=
							if ord(h)=ord(#"0") then aux t
							else
							h::t

					val X =explode(A)
					in
					implode(aux X)
					end
 
				val A=reverse(explode(removeLeadingZeroes(M)))
				val B=reverse(explode(removeLeadingZeroes(N)))

				fun compare M N=
					let fun removeLeadingZeroes A=
					let fun aux []=[]
						|aux (h::t)=
						if ord(h)=ord(#"0") then aux t
						else
						h::t

					val X =explode(A)
					in
					implode(aux X)
					end

					val A=removeLeadingZeroes M
					val B=removeLeadingZeroes N
	
					fun compare1 ([],[])=0
						|compare1 (h1::t1,h2::t2) =
							if ord(h1)>ord(h2) then 1
							else if ord(h1)<ord(h2) then ~1
							else compare1(t1,t2)

					in
					if size(A)>size(B) then 1
					else if size(B)>size(A) then ~1
					else compare1 (explode(A),explode(B))
					end

			in
			if compare M N = 1 then
			removeLeadingZeroes(implode(reverse(subtract1(A,B,0))))
			else
			removeLeadingZeroes(implode(reverse(subtract1(B,A,0))))
			end
			
			val m=(size(implode(X))+1)div 2
			val X1=List.take(X,length(X)-m)
			val X0=List.drop(X,length(X)-m)
			val Y1=List.take(Y,length(Y)-m)
			val Y0=List.drop(Y,length(Y)-m)
			val Z2=karatsuba (implode(X1), implode(Y1))
			val Z0=karatsuba (implode(X0), implode(Y0))
			val Ztemp=add(Z2,Z0)
			val Zi=karatsuba( subtract(implode(X0),implode(X1)), subtract(implode(Y0),implode(Y1)))



		fun compare (M ,N)=
			let fun removeLeadingZeroes A=
				let fun aux []=[]
				|aux (h::t)=
				if ord(h)=ord(#"0") then aux t
				else
				h::t

				val X =explode(A)
				in
				implode(aux X)
				end

			val A=removeLeadingZeroes M
			val B=removeLeadingZeroes N

			fun compare1 ([],[])=0
				|compare1 (h1::t1,h2::t2) =
					if ord(h1)>ord(h2) then 1
					else if ord(h1)<ord(h2) then ~1
					else compare1(t1,t2)

			in
			if size(A)>size(B) then 1
			else if size(B)>size(A) then ~1
			else compare1 (explode(A),explode(B))
			end

		fun adjustZeros(A,m)=	
	let fun reverse []=[]
		|reverse (h::t)=(reverse t) @ [h];

		val P=reverse(explode(A))
	
		fun temp(P,m)=
			if m=0 then P
			else #"0"::temp(P,m-1)


	in 
		implode(reverse(temp(P,m)))
	end

		in
			if ((compare (implode(X0),implode(X1)))*(compare (implode(Y1),implode(Y0))))=1 then
				explode(add(add(adjustZeros(Z2,2*m),Z0),adjustZeros(add(Ztemp,Zi),m)))
			else
				explode(add(add(adjustZeros(Z2,2*m),Z0),adjustZeros(subtract(Ztemp,Zi),m)))
		end		

	in
		if((vali (explode(X)) = false) orelse (vali (explode(Y))=false)) then raise NotUnsignedIntegers
		else
		if size(X)>size(Y) then
			implode(karatsuba1(explode(X),reverse(explode(adjustZeros(implode(reverse (explode(Y))),size(X)-size(Y))))))
			
		else
			implode(karatsuba1(reverse(explode(adjustZeros(implode(reverse (explode(X))),size(Y)-size(X)))),explode(Y)))
	end