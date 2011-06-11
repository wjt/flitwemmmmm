{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import qualified Data.Text as Text
import Data.Text (Text)
import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           Generator

import           Control.Monad.Reader
import           Control.Monad.Random (evalRandIO)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

data PhotoInfo =
    PhotoInfo { photoid, secret, server, farm, title :: String }

-- All of GlitchBot's photos to 2011-06-10
photos :: Seq PhotoInfo
photos = Seq.fromList
    [ PhotoInfo { photoid="5818764356", secret="52d47b5b74", server="2517", farm="3", title="@7E;[GU+.yID%{AmDd" }
    , PhotoInfo { photoid="5815176881", secret="5545d0dcc2", server="5154", farm="6", title="#:I^+vI;vX-X{^|Ru." }
    , PhotoInfo { photoid="5812494338", secret="1fa3de598f", server="5195", farm="6", title="tfv5p\\5#ty8YqkZ}5P" }
    , PhotoInfo { photoid="5808460487", secret="00eec67d27", server="3331", farm="4", title="$fbDoK636fZvTt)T)8" }
    , PhotoInfo { photoid="5704144616", secret="c4733f2663", server="5147", farm="6", title="DuCj9wY<Qjs?Q%vB+$" }
    , PhotoInfo { photoid="5699653151", secret="05928e0a30", server="2097", farm="3", title="]!bui@v6jHQ}u32`2" }
    , PhotoInfo { photoid="5696756252", secret="6b468bf4bc", server="3508", farm="4", title="8~9ubyuZ\"lwd{=aw" }
    , PhotoInfo { photoid="5693861180", secret="0888559db4", server="5306", farm="6", title="Px\\8&h+zEjqpumb>8" }
    , PhotoInfo { photoid="5690932410", secret="77626b03f2", server="5224", farm="6", title="_`cuvFz0kw@&>':@'" }
    , PhotoInfo { photoid="5687737552", secret="3ce60054d6", server="5149", farm="6", title=",O<9V*IR6ugi4z*;Rs" }
    , PhotoInfo { photoid="5684432324", secret="94b1ab3c58", server="5062", farm="6", title="5h3k9:b9yU(yYzrkO" }
    , PhotoInfo { photoid="5680162007", secret="c028cc3531", server="5144", farm="6", title="?.D3Ui9>T+%g'-`&$T" }
    , PhotoInfo { photoid="5676555262", secret="ce91c461d2", server="5026", farm="6", title="j6EElAWct[m~L}*zjN" }
    , PhotoInfo { photoid="5672466047", secret="af7e1e782c", server="5308", farm="6", title="iV3snTY,JR{Nsb5e7l" }
    , PhotoInfo { photoid="5669648991", secret="d90747bf9d", server="5064", farm="6", title="BcaYXIaCl~&:|}+f\\p" }
    , PhotoInfo { photoid="5665369114", secret="d2ca0d07ec", server="5304", farm="6", title="*+MT\"Y*EzsJE`+mcuC" }
    , PhotoInfo { photoid="5661306123", secret="fd7c28e3c7", server="5102", farm="6", title="?,_i`R<.]!FZ0A*W24" }
    , PhotoInfo { photoid="5658376302", secret="094aa36ced", server="5029", farm="6", title="o$p$9,\"VsfSE`sBA{" }
    , PhotoInfo { photoid="5654468702", secret="19c22640cf", server="5021", farm="6", title="\\O/j>@jU7YP\\'%ND'0" }
    , PhotoInfo { photoid="5649740777", secret="3b80d949d1", server="5264", farm="6", title="]h#1#t#4?LK9:0|Pf(" }
    , PhotoInfo { photoid="5646319929", secret="068774d0c0", server="5103", farm="6", title="Nk(:EJ>Aqg]:U*m3<W" }
    , PhotoInfo { photoid="5643342857", secret="5b5c335555", server="5109", farm="6", title="+&C2XJ{@Na[AeM/*iR" }
    , PhotoInfo { photoid="5640570505", secret="24b01f9a32", server="5145", farm="6", title=">vMu9f$]}~oh+I!:-Z" }
    , PhotoInfo { photoid="5638305546", secret="18b038e758", server="5264", farm="6", title="Nq<eKUl%]D~%Dn]tc%" }
    , PhotoInfo { photoid="5635320210", secret="b812ca80e5", server="5186", farm="6", title="d~74R:sc_t<={;p<Mo" }
    , PhotoInfo { photoid="5631986168", secret="6513c76199", server="5309", farm="6", title="q[SuoC51,z.x&5!KU3" }
    , PhotoInfo { photoid="5628197092", secret="8f7f74c9c2", server="5221", farm="6", title="[j!rh.k+'v}Cbwx%5]" }
    , PhotoInfo { photoid="5625019472", secret="835587e257", server="5021", farm="6", title=".oFHB<U%&yRNi}KqW*" }
    , PhotoInfo { photoid="5622375882", secret="bde1920e39", server="5029", farm="6", title="o0.nV-Gm^n3+t6*&E" }
    , PhotoInfo { photoid="5619137005", secret="426a89c83f", server="5270", farm="6", title="T]LLyqrmWfxY_*%'r" }
    , PhotoInfo { photoid="5616299443", secret="d1fb790153", server="5270", farm="6", title="cs'e=`)M`9Pf$aEv$x" }
    , PhotoInfo { photoid="5613275067", secret="f430f3bf22", server="5181", farm="6", title="Sp\\Gpb|[lD43XSpcCL" }
    , PhotoInfo { photoid="5610019593", secret="0412feaccb", server="5262", farm="6", title="ByEl<|t(;>rTu#`GM!" }
    , PhotoInfo { photoid="5606839154", secret="541a1d2830", server="4111", farm="5", title="v/2Sb\"d7'~4xc*zV_u" }
    , PhotoInfo { photoid="5603655180", secret="3549c52f72", server="5301", farm="6", title="F7qGC~;oEkB+rbpb9b" }
    , PhotoInfo { photoid="5600465949", secret="57d29c4345", server="5066", farm="6", title="d2;UjZqtlp|3M}tP*x" }
    , PhotoInfo { photoid="5597879709", secret="04a61bcfff", server="5142", farm="6", title="~y?<{5#vNreO#yu93" }
    , PhotoInfo { photoid="5595791748", secret="ba343544b8", server="5068", farm="6", title="qU\\q#.e6&~+lQ+t3WK" }
    , PhotoInfo { photoid="5592812384", secret="d2273a45f8", server="5029", farm="6", title="HaqXM]-K78sVrKQM(i" }
    , PhotoInfo { photoid="5589559704", secret="5cf1be896a", server="5094", farm="6", title="B%:k>_F.wJ:KMpMmSG" }
    , PhotoInfo { photoid="5585257171", secret="8e5169e9c5", server="5136", farm="6", title="xlYvHVd/G0m^Ps:n:7" }
    , PhotoInfo { photoid="5582664210", secret="b7438cc393", server="5068", farm="6", title="p$3Hv>bQ#O{77>WY/v" }
    , PhotoInfo { photoid="5579513653", secret="412ef19299", server="5056", farm="6", title="tqc\\i4{6usMgDY|qfN" }
    , PhotoInfo { photoid="5577474324", secret="2b73e0ede1", server="5095", farm="6", title="<jlIlNojm9?b_y/u&5" }
    , PhotoInfo { photoid="5574721458", secret="589658d4ee", server="5148", farm="6", title="Y@E4]Ux!T`f,Dr_ymP" }
    , PhotoInfo { photoid="5571839700", secret="55f400862a", server="5070", farm="6", title=")%U.\"8@Z\"]v)\\!~c>h" }
    , PhotoInfo { photoid="5568071881", secret="c9a073889b", server="5291", farm="6", title="d$5:@\"i!4#9hH6gV3e" }
    , PhotoInfo { photoid="5564333875", secret="9faf438731", server="5251", farm="6", title="3M5Cwr3xUY'A{J<,A{" }
    , PhotoInfo { photoid="5561672704", secret="f32d42cb16", server="5133", farm="6", title="4N7O0x*Fq8AL}3btci" }
    , PhotoInfo { photoid="5558482921", secret="0e03ff579a", server="5304", farm="6", title="bC!u2:srwn9LbmKq}b" }
    , PhotoInfo { photoid="5555779055", secret="00b3445298", server="5063", farm="6", title="7kS5l\";:ie%OU?(cV" }
    , PhotoInfo { photoid="5553003799", secret="4c106d899b", server="5067", farm="6", title="S=$R\"*m;Gh\\WtREf:&" }
    , PhotoInfo { photoid="5550663094", secret="4faf84d5ee", server="5254", farm="6", title="99U*7u70J<IsJo5&pV" }
    , PhotoInfo { photoid="5546849173", secret="b7ba409166", server="5135", farm="6", title="ZjRdJo<]e58n+:Fdi5" }
    , PhotoInfo { photoid="5543056735", secret="e6039ce416", server="5251", farm="6", title="Wxnbk.v./Q{#v:06t'" }
    , PhotoInfo { photoid="5539833455", secret="9de9a8d1c5", server="5259", farm="6", title="<1ap`ifqtma$b+bgm" }
    , PhotoInfo { photoid="5537807206", secret="761e9e2e56", server="5053", farm="6", title="h,n?,<F@*cMo^v[:!3" }
    , PhotoInfo { photoid="5534664417", secret="80f0341cda", server="5176", farm="6", title="MTZBo\\;.#5B\\(jYyG7" }
    , PhotoInfo { photoid="5531992527", secret="400c2d3c11", server="5171", farm="6", title="{{O=q-__w(rtZEeoa," }
    , PhotoInfo { photoid="5529791860", secret="340d2d07c2", server="5254", farm="6", title="3--K8#1n%s=1O[b-w9" }
    , PhotoInfo { photoid="5526797450", secret="e29ddbb39c", server="5138", farm="6", title="PGDXe,RL!f-&G;}$o" }
    , PhotoInfo { photoid="5523266854", secret="6586d1336d", server="5251", farm="6", title="R'YjE4:*M=[Y*7<'t~" }
    , PhotoInfo { photoid="5520395020", secret="b36e4cdafe", server="5012", farm="6", title="fxZsO]L35Hh7]T#!Jn" }
    , PhotoInfo { photoid="5517977226", secret="277101e993", server="5220", farm="6", title="3t@,[(eU]E|Xw,AQh*" }
    , PhotoInfo { photoid="5515478836", secret="d979fb0019", server="5014", farm="6", title=">D}8Km}9YI)8rORwqq" }
    , PhotoInfo { photoid="5512755566", secret="287c7d8927", server="5299", farm="6", title="L\"8gL|Yq8(Dg_6L(dG" }
    , PhotoInfo { photoid="5509932056", secret="7e131f9cd9", server="5254", farm="6", title="J{T}>K.+*[Y4%AI\\PZ" }
    , PhotoInfo { photoid="5506901858", secret="735deb7f17", server="5052", farm="6", title="lA%l/#qW+,e/$,jS39" }
    , PhotoInfo { photoid="5502694767", secret="cc764cdfe5", server="5096", farm="6", title=",PlAF%UD*Nd,@#RpfT" }
    , PhotoInfo { photoid="5500123814", secret="b3a20fa39f", server="5220", farm="6", title=",\\gfe4+D/eNs]Z@bQw" }
    , PhotoInfo { photoid="5496964349", secret="a8a6b56bac", server="5252", farm="6", title="[kDBM1BbQe)_sEe[\\u" }
    , PhotoInfo { photoid="5494389051", secret="f47843bf79", server="5132", farm="6", title="Km9#2J^W/}m%G\\Qe^x" }
    , PhotoInfo { photoid="5492407454", secret="f5d436db6d", server="5011", farm="6", title="LRnTM?q|~HJi-(Hv\\[" }
    , PhotoInfo { photoid="5489596434", secret="648c940513", server="5299", farm="6", title="fjX>8G.JJJU5J9TwT" }
    , PhotoInfo { photoid="5486442700", secret="23e57c7462", server="5178", farm="6", title="4#6OFzY6pVVzst'`C-" }
    , PhotoInfo { photoid="5482843716", secret="0603ed2157", server="5171", farm="6", title="YAXk7h@ruI%?I&u<:8" }
    , PhotoInfo { photoid="5479574808", secret="672cfc9bc2", server="5018", farm="6", title="Qv]+3duBX.C>Cfr1tC" }
    , PhotoInfo { photoid="5476419469", secret="be0ee5a187", server="5171", farm="6", title="g\\^vSDBJv`8RF6xOn_" }
    , PhotoInfo { photoid="5473803861", secret="6cd2c701c6", server="5131", farm="6", title="Grn$rJ{JRG%-fjBOd" }
    , PhotoInfo { photoid="5471741300", secret="8ae9fc8dec", server="5178", farm="6", title="D+kY1OpILlqgsmqU[\"" }
    , PhotoInfo { photoid="5468921190", secret="55b26b0a4f", server="5215", farm="6", title="%fH=?0xqKQ7Gis?Ul|" }
    , PhotoInfo { photoid="5465102721", secret="82268918a3", server="5097", farm="6", title="V`anGg?m'3/{w`;4@}" }
    , PhotoInfo { photoid="5461645441", secret="d783488f13", server="5053", farm="6", title="VXR8M_ni?%|R8,F\"@B" }
    , PhotoInfo { photoid="5459156988", secret="0f5c8873c6", server="5260", farm="6", title="_C&$aARTnEy=ZJh7ML" }
    , PhotoInfo { photoid="5456034129", secret="a565c24af4", server="5014", farm="6", title="[TTGJ7D@v(nz*$8Z%S" }
    , PhotoInfo { photoid="5454117658", secret="91fe562acb", server="5053", farm="6", title="reMrU1V0![rgLm(L8a" }
    , PhotoInfo { photoid="5450889423", secret="979524559d", server="5173", farm="6", title="wn!?ZCKCz&Kg\"=x[:" }
    , PhotoInfo { photoid="5448128697", secret="dc19910297", server="5135", farm="6", title="Z%3:s%1w<67WM84[q>" }
    , PhotoInfo { photoid="5445853382", secret="ff95c4b931", server="5018", farm="6", title="DQ6Y+j7{0a<3hXs4B@" }
    , PhotoInfo { photoid="5442421862", secret="7a77a6ca4e", server="4145", farm="5", title="RF@Y`Tt`Y%Ts)|fRGo" }
    , PhotoInfo { photoid="5438697053", secret="dd5d32df21", server="5297", farm="6", title="wc2jBo\"*Q2h-TT\"L'~" }
    , PhotoInfo { photoid="5436817568", secret="d1e970a170", server="5295", farm="6", title="]jkvS#P/FaE-yVs[B6" }
    , PhotoInfo { photoid="5434202944", secret="999e5e58b6", server="4146", farm="5", title="[}[]8}Yb^&t#yxtm~3" }
    , PhotoInfo { photoid="5430982685", secret="3896c9733b", server="4138", farm="5", title="Yw{|.(h$,.Ul0AU'9g" }
    , PhotoInfo { photoid="5428802246", secret="f65f979431", server="5175", farm="6", title="kZF,X04q:&b=\"UF;od" }
    , PhotoInfo { photoid="5425160823", secret="f8747ed160", server="5093", farm="6", title="s1d.fr[<Lz)F8vaqDg" }
    , PhotoInfo { photoid="5421757259", secret="eba7751ba5", server="5252", farm="6", title="rO?da#nQ6.^>{1Qisa" }
    , PhotoInfo { photoid="5419194052", secret="e296018935", server="5019", farm="6", title="zoo4EL&CQqPiK4uFlE" }
    , PhotoInfo { photoid="5415905889", secret="fc113555dc", server="4100", farm="5", title="([_91xTpF0\"1e<-(=z" }
    , PhotoInfo { photoid="5413269479", secret="6ca90c2a8e", server="5258", farm="6", title="Kqe:}2!YFSY9kw~[gq" }
    , PhotoInfo { photoid="5410518035", secret="a92b2ef064", server="5292", farm="6", title="Z4G4\\Bs\\TB$+Q|Kf-V" }
    , PhotoInfo { photoid="5408283354", secret="1215de2d6a", server="5257", farm="6", title="f<#x*W!l.]^KF-]tC+" }
    , PhotoInfo { photoid="5405262916", secret="6540775c59", server="5177", farm="6", title="*=19})PrF&8owH#JpA" }
    , PhotoInfo { photoid="5401799558", secret="9df71cc99c", server="5134", farm="6", title="RCsK>AhLk|c^TyTyOz" }
    , PhotoInfo { photoid="5398714358", secret="8152b7f3e2", server="5211", farm="6", title="uOe[L~2Nf#IC_7{E!" }
    , PhotoInfo { photoid="5396212182", secret="182ce9e162", server="4078", farm="5", title="Lcs\"!k78zr(,p`P9rK" }
    , PhotoInfo { photoid="5393591380", secret="727ff7dcb6", server="5294", farm="6", title="\"ZEP!@?0toar.='<7" }
    , PhotoInfo { photoid="5390292387", secret="d2b9ecef98", server="5297", farm="6", title="HJBXW\\9F8E&O3m0dKR" }
    , PhotoInfo { photoid="5388120046", secret="b4b62e6b10", server="5214", farm="6", title="~8,[cG)d.?.X?\"-53h" }
    , PhotoInfo { photoid="5385200666", secret="432daf27d3", server="5217", farm="6", title="U*Ifa=8hI:{,e'C(tc" }
    , PhotoInfo { photoid="5381077485", secret="c48ac5a1a3", server="5125", farm="6", title="1}ES9eH_SolW<y{efC" }
    , PhotoInfo { photoid="5378554068", secret="15810f0a2a", server="5248", farm="6", title="P`)QV]P-1Ir,#pjD=x" }
    , PhotoInfo { photoid="5375988634", secret="caa45e3590", server="5161", farm="6", title="QgF#{{X@]Q**X/0B.4" }
    , PhotoInfo { photoid="5372745891", secret="630fa07dc4", server="5126", farm="6", title="4fhbJZ_A}][Az{Ly.[" }
    , PhotoInfo { photoid="5370053241", secret="7237a4766b", server="5165", farm="6", title="Q=vGLKHHta\"t1&OC>H" }
    , PhotoInfo { photoid="5367152737", secret="36bed7b7d3", server="5004", farm="6", title="h5e|Lm}WNvu!mF<\\A" }
    , PhotoInfo { photoid="5364468074", secret="f39c3a048f", server="5123", farm="6", title="H7yt`qI3${ku,lH1uN" }
    , PhotoInfo { photoid="5360353245", secret="d96e2b4e1f", server="5161", farm="6", title="uDkUc5Y{.G;V&a,2}" }
    , PhotoInfo { photoid="5357753914", secret="d1ec28f142", server="5130", farm="6", title="}~,,D$4b9[yGR:(TYM" }
    , PhotoInfo { photoid="5354508525", secret="fb4a8e7dc8", server="5285", farm="6", title="#3xkK6c\"z`7*FMq0W=" }
    , PhotoInfo { photoid="5352549538", secret="dc8118f09f", server="5122", farm="6", title=".n}fa'Z|OALPuq|K\"" }
    , PhotoInfo { photoid="5349153509", secret="0d7d146a39", server="5045", farm="6", title=":G*IL[s#N)roKg9H>e" }
    , PhotoInfo { photoid="5346864086", secret="e233ce3a5c", server="5164", farm="6", title="P.`IDm*|Cy+TAB6Vw" }
    , PhotoInfo { photoid="5343042849", secret="9fdd386f6d", server="5128", farm="6", title="plP.Ckd`\\~RE:mz|CC" }
    , PhotoInfo { photoid="5339335385", secret="0f8543ce9c", server="5084", farm="6", title="\"YyZVy]\\a\\|:Afyg{:" }
    , PhotoInfo { photoid="5335958745", secret="7494fa9187", server="5124", farm="6", title="`UrBT67\\\">thSyT4Xs" }
    , PhotoInfo { photoid="5333127871", secret="d464f07e72", server="5288", farm="6", title="8`Zf7F8{Od<Gk];yS}" }
    , PhotoInfo { photoid="5330855346", secret="59fc4ae951", server="5049", farm="6", title="n7l;bz/X<C`q9$ByWC" }
    , PhotoInfo { photoid="5327831548", secret="b3ca573781", server="5287", farm="6", title="^`;K&*ojkAPxbt>R?z" }
    , PhotoInfo { photoid="5324519530", secret="c8afb00430", server="5081", farm="6", title="G&o@9c)%PC9Mso.'s" }
    , PhotoInfo { photoid="5320919102", secret="ae598682db", server="5089", farm="6", title=",hWZo]+k5vA=UyjVPx" }
    , PhotoInfo { photoid="5316239421", secret="762e70892b", server="5082", farm="6", title="<\"'vE5'c49ZoQHEjbM" }
    , PhotoInfo { photoid="5313019008", secret="f292554924", server="5248", farm="6", title="PWl1aPED@TV.QXrQiG" }
    , PhotoInfo { photoid="5309675603", secret="cb7e03dd6e", server="5210", farm="6", title="dphkv7R#N!B<v~,Sc!" }
    , PhotoInfo { photoid="5307245926", secret="55a8b51607", server="5285", farm="6", title="8lH%s>@(H[aBDEj<l%" }
    , PhotoInfo { photoid="5304038718", secret="f5563a6bbc", server="5285", farm="6", title="hxYJh?BOg53ve%i]8" }
    , PhotoInfo { photoid="5300838602", secret="4149c455b3", server="5089", farm="6", title="Z@o#pNf<S7Lj|po_xJ" }
    , PhotoInfo { photoid="5296860453", secret="6a02a42d59", server="5090", farm="6", title="i/h`A$N<TJT9~dNi\"#" }
    , PhotoInfo { photoid="5293994336", secret="1dc8ee0a0a", server="5089", farm="6", title="COuN`\"*<bggP>CjlFc" }
    , PhotoInfo { photoid="5290333745", secret="678a6c4bb7", server="5006", farm="6", title="a\\_=E=.5|)d&]\"}7vG" }
    , PhotoInfo { photoid="5288544044", secret="84ff2c99be", server="5090", farm="6", title="`[9kqG\"_H8&xWqd`R:" }
    , PhotoInfo { photoid="5285551177", secret="353fc1a4f2", server="5006", farm="6", title="6r:l^A=E4\\>{EDMl^T" }
    , PhotoInfo { photoid="5282971619", secret="e7826a9459", server="5202", farm="6", title="F(Efm306\\k/1QP%:bN" }
    , PhotoInfo { photoid="5280920552", secret="15ab7dd05e", server="5123", farm="6", title="_Q\\5@N5jIQ`+FR:l%" }
    , PhotoInfo { photoid="5278017478", secret="b1f3c74742", server="5003", farm="6", title="WkM1gj8/:JEQ=\"9PN6" }
    , PhotoInfo { photoid="5274746124", secret="91722941c3", server="5170", farm="6", title="StPzb/-y&YX\"Bgybzu" }
    , PhotoInfo { photoid="5271196499", secret="167a112d0c", server="5044", farm="6", title=";5`E#$\\,0X]*s^$9jv" }
    , PhotoInfo { photoid="5269300794", secret="4302ef6487", server="5289", farm="6", title="eT.g8BY03qq-whCm0%" }
    , PhotoInfo { photoid="5266784146", secret="b177fd927a", server="5090", farm="6", title="}`x0,6J_cY[tpf&&t\\" }
    , PhotoInfo { photoid="5264170678", secret="a2e4cb4838", server="5287", farm="6", title="V;o+7UgHV7z\\x=:j;&" }
    , PhotoInfo { photoid="5261414254", secret="18fa614265", server="5006", farm="6", title="qf#kbZ8SXfV4:NI?P" }
    , PhotoInfo { photoid="5257847159", secret="18e378b43c", server="5210", farm="6", title="Q0a43PEk}l7r~w*}Tx" }
    , PhotoInfo { photoid="5254408461", secret="08d86d6256", server="5166", farm="6", title="*)\"VRJ|p\\`fpQ3HCm~" }
    , PhotoInfo { photoid="5252054292", secret="150b99485b", server="5163", farm="6", title="N?1:UQP`p5m3jH-,i!" }
    , PhotoInfo { photoid="5249000273", secret="a6d9dbcc37", server="5284", farm="6", title="2-bNZI;aP]n&E/Q$0_" }
    , PhotoInfo { photoid="5247120146", secret="1c5cb17ef7", server="5124", farm="6", title="RyYjal3j=fGW\"1/;bW" }
    , PhotoInfo { photoid="5243960749", secret="6025d4f402", server="5128", farm="6", title="[\\#1A6faA7!g4;g~go" }
    , PhotoInfo { photoid="5241205347", secret="cd401e924d", server="5121", farm="6", title="hJA,ycXxWN{uDb8RZk" }
    , PhotoInfo { photoid="5238165449", secret="22660848cc", server="5168", farm="6", title="k1]3A+^*):7yyc\\0fp" }
    , PhotoInfo { photoid="5234659103", secret="63f29990ac", server="5283", farm="6", title="D[KVHc9wp<t+P?fm={" }
    , PhotoInfo { photoid="5231558673", secret="c92efb7d14", server="5088", farm="6", title="R4<|0/+{{vO.pa)EO" }
    , PhotoInfo { photoid="5229014461", secret="1dc9866812", server="5245", farm="6", title="++'mA:V294Y}IGF3SK" }
    , PhotoInfo { photoid="5226406299", secret="9648e465c1", server="5126", farm="6", title="d?`4BBrH`N2p$2dZ1@" }
    , PhotoInfo { photoid="5223751599", secret="b797040beb", server="5287", farm="6", title="eVg/}xr/XJ9NQk{,Jy" }
    , PhotoInfo { photoid="5220960647", secret="e214bfcccf", server="5042", farm="6", title="Zbk6D}vJ,u{7:^zkb;" }
    , PhotoInfo { photoid="5218602050", secret="be4485fcb4", server="5089", farm="6", title="z_jXLv\"[p@br`q]bK{" }
    , PhotoInfo { photoid="5215208386", secret="86e62b924c", server="5167", farm="6", title="Fgh4GshZv,T3S#M_zM" }
    , PhotoInfo { photoid="5212206988", secret="40719450bc", server="5245", farm="6", title="\")^R)Q&SogrjRr]H;c" }
    , PhotoInfo { photoid="5209062983", secret="e9394ea681", server="5209", farm="6", title="J2%&+j)~#.hOhsQR}+" }
    , PhotoInfo { photoid="5206654421", secret="20ef416609", server="5242", farm="6", title="7GjFPDE_xWch'cMEco" }
    , PhotoInfo { photoid="5204811016", secret="527e51fabe", server="5121", farm="6", title="LoosC[*-59#TA|(DY]" }
    , PhotoInfo { photoid="5202138102", secret="6aabb00a67", server="5284", farm="6", title="iqA?Hw^[(PBAA{x1Wp" }
    , PhotoInfo { photoid="5199098144", secret="005b8cb0c5", server="4111", farm="5", title="IIl*Z(\\5GZfd{wl-`" }
    , PhotoInfo { photoid="5195126677", secret="af90e48b8f", server="4088", farm="5", title="V6&R;Lnsb6:Nz[U$\"M" }
    , PhotoInfo { photoid="5192689638", secret="37f5cc75d0", server="4131", farm="5", title="{G'#r+a}$%0!^M\\<zP" }
    , PhotoInfo { photoid="5189625379", secret="e634012596", server="1168", farm="2", title="~$?NGlYLAUsjF;,c'o" }
    , PhotoInfo { photoid="5187182685", secret="6cfe14937b", server="4091", farm="5", title="A,-kD%&C:'qK\\@rR3Y" }
    , PhotoInfo { photoid="5185172186", secret="f929f9cfe2", server="1425", farm="2", title="3`aK9WsXZ2NF,[5=H" }
    , PhotoInfo { photoid="5181804077", secret="f84cd9d396", server="4151", farm="5", title="z&&U1zzz8^iPixBk+" }
    , PhotoInfo { photoid="5179361234", secret="3a996e106d", server="1425", farm="2", title="9\\&<G&HQ'$21My1(2S" }
    , PhotoInfo { photoid="5175062455", secret="31ee585777", server="4148", farm="5", title="Nag;>6]?^TkWRYstF@" }
    , PhotoInfo { photoid="5171877319", secret="930d97016b", server="4103", farm="5", title="458(wOkN+)x_z_jfD^" }
    , PhotoInfo { photoid="5169386639", secret="d50e05372a", server="1255", farm="2", title="?UUZW/gK6A:0(RWb3)" }
    , PhotoInfo { photoid="5166782673", secret="818c13315e", server="4008", farm="5", title="KZ`cS~j#-^P9':@9I1" }
    , PhotoInfo { photoid="5164177461", secret="1a39c616a9", server="4061", farm="5", title="\"Mv&e.CoU\"'ERJe2tg" }
    , PhotoInfo { photoid="5161375779", secret="f8cfac77f9", server="1098", farm="2", title=";k&:~:Y[gXe'Cu5g;b" }
    , PhotoInfo { photoid="5158881846", secret="9b34f589ed", server="4153", farm="5", title="J4sJm6r>u\"Z8#vOgST" }
    , PhotoInfo { photoid="5155185066", secret="310c1e8c9c", server="1235", farm="2", title="S=.o\"M4:#1o9L*Ffo:" }
    , PhotoInfo { photoid="5151111895", secret="e713759dfa", server="4047", farm="5", title="xA?X&vxlw!Bb843\\81" }
    , PhotoInfo { photoid="5148504311", secret="6b56ce7950", server="4018", farm="5", title="@a1n:yDMC^\"XRzAT?V" }
    , PhotoInfo { photoid="5145778263", secret="60bb5865c1", server="4065", farm="5", title="|##w\\W&$*!Oyn5e^=<" }
    , PhotoInfo { photoid="5142856287", secret="3b1321ec76", server="1094", farm="2", title="962ij,Gk)A!q8w8)(y" }
    , PhotoInfo { photoid="5139716149", secret="fff6dc5faa", server="1079", farm="2", title="Qm'nwMTP*3m!J'zyx!" }
    , PhotoInfo { photoid="5136058775", secret="a2901036c2", server="4083", farm="5", title=".fd%MK6NaoL5dgxPa>" }
    , PhotoInfo { photoid="5132467072", secret="2facb825c6", server="1415", farm="2", title=".wff-pRGP=oO+IU>]X" }
    , PhotoInfo { photoid="5129146170", secret="bd0c73cd9e", server="4007", farm="5", title="s_!cjksm*p)P[MixA(" }
    , PhotoInfo { photoid="5126476940", secret="3b276f70c2", server="1101", farm="2", title="l.t-6`%6&JhL6$|fYt" }
    , PhotoInfo { photoid="5123213511", secret="460127ec6c", server="4012", farm="5", title="iQbO3]KZv,c)l29V#j" }
    , PhotoInfo { photoid="5120549531", secret="81e2a9740a", server="4021", farm="5", title="-[}'1DRX^]2Y7C\"LB_" }
    , PhotoInfo { photoid="5118279212", secret="24fe74d1eb", server="1199", farm="2", title="d3U~1AC|pSK$y1&6=W" }
    , PhotoInfo { photoid="5114410291", secret="5cb4159edf", server="1427", farm="2", title="ZuqVgHI!A8r,![{7Wi" }
    , PhotoInfo { photoid="5111255280", secret="8418e01e66", server="1231", farm="2", title="{p&|UTdowG:;jpBRB;" }
    , PhotoInfo { photoid="5107414385", secret="e13ae1e175", server="1151", farm="2", title="Sy8#zvlX>T@;r\"#OCu" }
    , PhotoInfo { photoid="5105425608", secret="d1c097b5f4", server="4129", farm="5", title="kJB_/@M<:p#6xm6B0)" }
    , PhotoInfo { photoid="5102352939", secret="c36bcf3f44", server="1092", farm="2", title="]Z.;uSeFLnsN\\p?_p" }
    , PhotoInfo { photoid="5100044752", secret="2ce0b60c3f", server="4152", farm="5", title="Q57()^OQ.ZZ/:+>:gp" }
    , PhotoInfo { photoid="5097294396", secret="501f687a56", server="4107", farm="5", title="E~=8TWcOo5m#hqa:E]" }
    , PhotoInfo { photoid="5093470411", secret="98f0b2677b", server="4089", farm="5", title="M!v4Gq@pG>Geo_}4:K" }
    , PhotoInfo { photoid="5090168668", secret="7b041abc6c", server="4112", farm="5", title="6?`};4\\O^SswA^E[3g" }
    , PhotoInfo { photoid="5086952312", secret="d868e52cb3", server="4144", farm="5", title="{B(8(SX=#'F~CE1=~=" }
    , PhotoInfo { photoid="5083729907", secret="1f71e155a0", server="4106", farm="5", title="cUrTnB_5Q\"MZOp.hs;" }
    , PhotoInfo { photoid="5081676612", secret="ea5acdd9b6", server="4112", farm="5", title="$Z|dW{\\i$5x@POO\"5" }
    , PhotoInfo { photoid="5078877158", secret="1cd7fc1c21", server="4061", farm="5", title="vXVf:128M1P}bBHz@1" }
    , PhotoInfo { photoid="5075856298", secret="bacf87ce94", server="4060", farm="5", title="\"L.%>(wtT{-K=h+Oq1" }
    , PhotoInfo { photoid="5072364590", secret="12cb12c749", server="4089", farm="5", title="Rz5GT'eNw?cYZi)tBI" }
    , PhotoInfo { photoid="5067859261", secret="abfeee95a7", server="4112", farm="5", title="/,sOJY0a0-E4-6Oh#%" }
    , PhotoInfo { photoid="5064784925", secret="0658630129", server="4110", farm="5", title="[1u!L{{'7am}`+bjO" }
    , PhotoInfo { photoid="5062916248", secret="4483affe2a", server="4083", farm="5", title="{|/`QDTV`-=;:W;C" }
    , PhotoInfo { photoid="5059769935", secret="ff9fd956a3", server="4133", farm="5", title="DFZmn27*ogtP;rSPU-" }
    , PhotoInfo { photoid="5057110505", secret="7a008e01e9", server="4150", farm="5", title="(CHmi9zAJ;yNh;3qS`" }
    , PhotoInfo { photoid="5054888168", secret="9e600ca331", server="4146", farm="5", title="eze6SknCs*2?|5K6'=" }
    , PhotoInfo { photoid="5051722078", secret="563c63faf1", server="4091", farm="5", title="ob7ag$\\y(~Gm#=yt@" }
    , PhotoInfo { photoid="5048020648", secret="1e1d1b578a", server="4152", farm="5", title="{U7fJ6#z))\"F*=I_3" }
    , PhotoInfo { photoid="5044821744", secret="8a49ac8306", server="4127", farm="5", title="L(DXw[a:fv1S@8|R?+" }
    , PhotoInfo { photoid="5041640065", secret="ba249751b9", server="4127", farm="5", title="y5E92Jk2\\kPl>bV([" }
    , PhotoInfo { photoid="5038975407", secret="19cebee4d6", server="4108", farm="5", title="5|]suKC_[?:q@{8.So" }
    , PhotoInfo { photoid="5036809254", secret="88163b51d3", server="4084", farm="5", title="vu>XXx|$y'vx{nl|%)" }
    , PhotoInfo { photoid="5033824142", secret="d078a48fb2", server="4144", farm="5", title="yk='wKQ.K:$Hykl'=C" }
    , PhotoInfo { photoid="5029843799", secret="fd90c60e11", server="4127", farm="5", title="{!k'\\/k2pBiW$=Av4w" }
    , PhotoInfo { photoid="5026786312", secret="ca7ac8367c", server="4087", farm="5", title="+a>@|G#K~R[.Y=IX]w" }
    , PhotoInfo { photoid="5023562112", secret="40b7d2a68c", server="4083", farm="5", title="G`zL$vZJqIBi-BCR35" }
    , PhotoInfo { photoid="5020940478", secret="1b78b14c91", server="4112", farm="5", title="}Hm[cUBEdoY%vD!YcK" }
    , PhotoInfo { photoid="5017594729", secret="7b242b1d7f", server="4125", farm="5", title="}'#TfLbGDBq[B5`jwN" }
    , PhotoInfo { photoid="5014705145", secret="7388e058f4", server="4148", farm="5", title="1DS}QHx`*5desz:UQO" }
    , PhotoInfo { photoid="5011722547", secret="e04bdab073", server="4146", farm="5", title="3Gq<0{z78pFu0(^_g" }
    , PhotoInfo { photoid="5008995744", secret="2490f32476", server="4089", farm="5", title="&E9GK}pyWzYd9/#g(" }
    , PhotoInfo { photoid="5005106102", secret="f4e342a650", server="4085", farm="5", title="?&@~V64R>dHwsq&P7O" }
    , PhotoInfo { photoid="5001180877", secret="b3da497705", server="4113", farm="5", title="Q3>sxlALwEvd?P3Op/" }
    , PhotoInfo { photoid="4998547285", secret="6d323ec9b7", server="4152", farm="5", title="]eR+!BBir2),h0iMy:" }
    , PhotoInfo { photoid="4996623898", secret="ccae794f99", server="4132", farm="5", title="OoFdJD`gbIK0!L[hdI" }
    , PhotoInfo { photoid="4987282358", secret="018312c313", server="4111", farm="5", title="G]9LjoO[\"7\"NPe_`Mf" }
    , PhotoInfo { photoid="4983419250", secret="7a96ed2524", server="4112", farm="5", title="\"m@&-`l'b\"i!\\|{+o" }
    , PhotoInfo { photoid="4980092078", secret="be83e7a303", server="4146", farm="5", title="9P>Z_c-#YMk^hxw'1R" }
    , PhotoInfo { photoid="4976758287", secret="afd1fd3ae6", server="4128", farm="5", title="p#y'`t{SKQU=|Ssr-" }
    , PhotoInfo { photoid="4974570638", secret="8e91b52141", server="4088", farm="5", title="q:V)A6QTm3s1oqUkK" }
    , PhotoInfo { photoid="4970997219", secret="db3789ed70", server="4113", farm="5", title="Z%6J'ag%s0}NwP>PbU" }
    , PhotoInfo { photoid="4967738067", secret="d124cc6b9d", server="4154", farm="5", title="-l{/KleQ26CL1|VJX," }
    , PhotoInfo { photoid="4963910811", secret="442d861ef7", server="4129", farm="5", title=")H,R;rTCR_(ZwcnH\"z" }
    , PhotoInfo { photoid="4960852294", secret="b8c8774069", server="4147", farm="5", title=";cI]xpM#O:r;)3Q3f" }
    , PhotoInfo { photoid="4956994267", secret="d349494343", server="4083", farm="5", title="9a&}lL_CMrm}236wJ[" }
    , PhotoInfo { photoid="4954737986", secret="790a285848", server="4133", farm="5", title="-fLhI`b%--0F,hS29" }
    , PhotoInfo { photoid="4951295375", secret="a6f664bde2", server="4152", farm="5", title="jKJo>\"^j?v0+)_7O0/" }
    , PhotoInfo { photoid="4948342429", secret="b7fc03da79", server="4151", farm="5", title="~gfN\"PW]j,?6dw_\"ip" }
    , PhotoInfo { photoid="4945204699", secret="dcc77344cc", server="4109", farm="5", title="*?,y'YI(t}Y?]FR8W" }
    , PhotoInfo { photoid="4941811225", secret="c6b1fab4ef", server="4121", farm="5", title="v_11j$z'VrBlOvQyr}" }
    , PhotoInfo { photoid="4937970499", secret="d6acc53a94", server="4096", farm="5", title="T4d_EMiS5ID)4MX-1t" }
    , PhotoInfo { photoid="4935312092", secret="16560fcc75", server="4134", farm="5", title="T`$]uR@'k*o%F+\"GIx" }
    , PhotoInfo { photoid="4932618120", secret="2b4970e7e9", server="4081", farm="5", title="`]}&Hde)H=hBjLK(`" }
    , PhotoInfo { photoid="4929307251", secret="65eea12398", server="4141", farm="5", title="NMn@g(?T]m#l\\bN|ep" }
    , PhotoInfo { photoid="4927085636", secret="860aa22d21", server="4079", farm="5", title="/~l$9!L*LRdH\"4}fR" }
    , PhotoInfo { photoid="4923475877", secret="1e2825ea5c", server="4137", farm="5", title="B7'P0dT&TvX&$NayFg" }
    , PhotoInfo { photoid="4920101419", secret="35fa744d9d", server="4098", farm="5", title="Jm$_7?COx+<{06Rd1]" }
    , PhotoInfo { photoid="4916262461", secret="f2e4e9c40a", server="4118", farm="5", title="}1Y~$'!K7ls\"{ogEB4" }
    , PhotoInfo { photoid="4913606648", secret="2df1d03751", server="4076", farm="5", title="]=R#(Eyb3aU\"rOSz8R" }
    , PhotoInfo { photoid="4910905542", secret="bd3247de16", server="4143", farm="5", title="E\\ta~!1:@-$Ay-_o,e" }
    , PhotoInfo { photoid="4907448013", secret="6a6f1944a5", server="4081", farm="5", title=".t\\8A!X'6,g7aB,07i" }
    , PhotoInfo { photoid="4904481877", secret="76a1d01bde", server="4139", farm="5", title="T'WiOY#lM9S9bu)pJp" }
    , PhotoInfo { photoid="4901906206", secret="ee381fbfaf", server="4141", farm="5", title="?\"4/|lsDg{F1ezFV4>" }
    , PhotoInfo { photoid="4897866461", secret="8959ea505e", server="4123", farm="5", title="afx%\"/L)w{CQ|gBH-7" }
    , PhotoInfo { photoid="4894610404", secret="2c04e8873d", server="4123", farm="5", title="qHD'$PF[JXrNifJC,;" }
    , PhotoInfo { photoid="4888666968", secret="77c1abd2c4", server="4134", farm="5", title="6WD?V(1m6{aT-1z-:2" }
    , PhotoInfo { photoid="4885841278", secret="f3553a7ffc", server="4074", farm="5", title="p#%WaCb!k3Czhku,v]" }
    , PhotoInfo { photoid="4882862438", secret="61cf40f131", server="4117", farm="5", title="Xl-H*}nQE}<$.Z]5Uu" }
    , PhotoInfo { photoid="4879168731", secret="3bf5208f03", server="4119", farm="5", title="A+^\\uGO8+$m;p%]\\%i" }
    , PhotoInfo { photoid="4876333556", secret="31dbaa0f7c", server="4120", farm="5", title="3s)oVI>|H4+US*Iqq" }
    , PhotoInfo { photoid="4872488772", secret="1687ddfe1c", server="4115", farm="5", title="_s~H=MGy/{'G/H%6t8" }
    , PhotoInfo { photoid="4868736275", secret="b1dd7e710a", server="4137", farm="5", title="/[Q9.j33Lm\\30ERdtk" }
    , PhotoInfo { photoid="4866607524", secret="166ea8deb9", server="4123", farm="5", title="EszMo,zY+lr}twgfYp" }
    , PhotoInfo { photoid="4863726266", secret="2663a6dbf8", server="4141", farm="5", title="-o$2/ad)*C(S7Jly0x" }
    , PhotoInfo { photoid="4860752156", secret="f6814c8806", server="4134", farm="5", title="^L3L@>m2ewpE<=D!'o" }
    , PhotoInfo { photoid="4857574762", secret="491f52cc3f", server="4094", farm="5", title="<mPZ$.:5:O!PU<'ihL" }
    , PhotoInfo { photoid="4853428933", secret="81124dbb08", server="4073", farm="5", title="7H5#8,-FbG7IoHoWDN" }
    , PhotoInfo { photoid="4849487329", secret="10ab1d7cdc", server="4134", farm="5", title="3Y\"^aM_8f43YR$1e=+" }
    , PhotoInfo { photoid="4846910012", secret="4b721ac2e0", server="4110", farm="5", title="q1&|FTil+#2Tn*>9E." }
    , PhotoInfo { photoid="4843596323", secret="09fbe0305f", server="4151", farm="5", title="ngZ[!'epLq7Bz8~wa}" }
    , PhotoInfo { photoid="4840750467", secret="857b5e5323", server="4153", farm="5", title="*aT7.YUd>gDTX_}H\"!" }
    , PhotoInfo { photoid="4838300732", secret="2727b0fa76", server="4151", farm="5", title="h!Mzc.+~TGBT+I>}C<" }
    , PhotoInfo { photoid="4834432999", secret="483c7206e0", server="4105", farm="5", title="|1\"H^X=1=?(MJ2v]qc" }
    , PhotoInfo { photoid="4831513104", secret="6805a9be1a", server="4144", farm="5", title="IY^=maC1O!PG|nL9\\b" }
    , PhotoInfo { photoid="4827550942", secret="5854bf2830", server="4099", farm="5", title="d@dwf@A6-UR>Yr^|eh" }
    , PhotoInfo { photoid="4823987491", secret="7f38413374", server="4136", farm="5", title="[*DT4kb5F(o3EuKxX" }
    , PhotoInfo { photoid="4821003333", secret="4219eef54b", server="4102", farm="5", title="\\oY~3n=(/y.9(r$2)G" }
    , PhotoInfo { photoid="4818641303", secret="8ce29f7621", server="4079", farm="5", title="1-!LX\"z&d%/${@,\"=y" }
    ]

photoUrl, photoPageUrl :: PhotoInfo -> Text
photoUrl photoInfo = Text.pack $ concat [ "http://farm"
                            , farm photoInfo
                            , ".static.flickr.com/"
                            , server photoInfo
                            , "/"
                            , photoid photoInfo
                            , "_"
                            , secret photoInfo
                            , "_m.jpg"
                            ]
photoPageUrl photoInfo = Text.pack $ glitchbotBase ++ photoid photoInfo
  where
    glitchbotBase = "http://www.flickr.com/photos/glitchbot/"

index :: Application ()
index = ifTop $ do
    model <- asks modelState
    (trackName, photoInfo) <- liftIO . evalRandIO $
        liftM2 (,) (inventName model) (randomElem photos)

    let bindings = [ ("track-name", trackName)
                   , ("photo-url", photoUrl photoInfo)
                   , ("photo-page-url", photoPageUrl photoInfo)
                   , ("photo-title", Text.pack $ title photoInfo)
                   ]

    heistLocal (bindStrings bindings) $ render "index"

site :: Application ()
site = route [ ("/",            index)
             ]
       <|> serveDirectory "resources/static"
