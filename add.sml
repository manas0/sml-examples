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