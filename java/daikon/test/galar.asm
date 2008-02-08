

ppt gklayout.dll:0x196dfb:::BB
 gklayout.dll:0x196dfb push esi esp -> esp [0+esp] 
 gklayout.dll:0x196dfc mov_ld [12+esp] -> esi 
 gklayout.dll:0x196e00 test esi esi 
 gklayout.dll:0x196e02 jnz_short $0x1f16e0b 


ppt gklayout.dll:0x196e04:::BB
 gklayout.dll:0x196e04 mov_imm $0x80004003 -> eax 
 gklayout.dll:0x196e09 pop esp [0+esp] -> esi esp 
 gklayout.dll:0x196e0a ret esp [0+esp] -> esp 


ppt gklayout.dll:0x196e0b:::BB
 gklayout.dll:0x196e0b push [8+esp] esp -> esp [0+esp] 
 gklayout.dll:0x196e0f push_imm $0x34 esp -> esp [0+esp] 
 gklayout.dll:0x196e11 call $0x1d8fe46 esp -> esp [0+esp] 


ppt gklayout.dll:0x196e16:::BB
 gklayout.dll:0x196e16 pop esp [0+esp] -> ecx esp 
 gklayout.dll:0x196e17 test eax eax 
 gklayout.dll:0x196e19 pop esp [0+esp] -> ecx esp 
 gklayout.dll:0x196e1a jz_short $0x1f16e25 


ppt gklayout.dll:0x196e1c:::BB
 gklayout.dll:0x196e1c mov_ld eax -> ecx 
 gklayout.dll:0x196e1e call $0x1f16e38 esp -> esp [0+esp] 


ppt gklayout.dll:0x196e23:::BB
 gklayout.dll:0x196e23 jmp_short $0x1f16e27 


ppt gklayout.dll:0x196e25:::BB
 gklayout.dll:0x196e25 xor eax eax -> eax 


ppt gklayout.dll:0x196e27:::BB
 gklayout.dll:0x196e27 test eax eax 
 gklayout.dll:0x196e29 jnz_short $0x1f16e32 


ppt gklayout.dll:0x196e2b:::BB
 gklayout.dll:0x196e2b mov_imm $0x8007000e -> eax 
 gklayout.dll:0x196e30 pop esp [0+esp] -> esi esp 
 gklayout.dll:0x196e31 ret esp [0+esp] -> esp 


ppt gklayout.dll:0x196e32:::BB
 gklayout.dll:0x196e32 mov_st eax -> [0+esi] 
 gklayout.dll:0x196e34 xor eax eax -> eax 
 gklayout.dll:0x196e36 pop esp [0+esp] -> esi esp 
 gklayout.dll:0x196e37 ret esp [0+esp] -> esp 


ppt gklayout.dll:0x196e38:::BB
 gklayout.dll:0x196e38 push esi esp -> esp [0+esp] 
 gklayout.dll:0x196e39 mov_ld ecx -> esi 
 gklayout.dll:0x196e3b call $0x1d955f6 esp -> esp [0+esp] 


ppt gklayout.dll:0x196e40:::BB
 gklayout.dll:0x196e40 mov_st $0x01f76a70 -> [0+esi] 
 gklayout.dll:0x196e46 mov_ld esi -> eax 
 gklayout.dll:0x196e48 pop esp [0+esp] -> esi esp 
 gklayout.dll:0x196e49 ret esp [0+esp] -> esp 


ppt gklayout.dll:0x196e9e:::BB
 gklayout.dll:0x196e9e mov_ld [33173960+] -> eax 
 gklayout.dll:0x196ea3 ret esp [0+esp] -> esp 


ppt gklayout.dll:0x196e4a:::BB
 gklayout.dll:0x196e4a mov_ld [12+esp] -> ecx 
 gklayout.dll:0x196e4e test ecx ecx 
 gklayout.dll:0x196e50 jnz_short $0x1f16e59 


ppt gklayout.dll:0x196e52:::BB
 gklayout.dll:0x196e52 mov_imm $0x80004003 -> eax 
 gklayout.dll:0x196e57 jmp_short $0x1f16e9b 


ppt gklayout.dll:0x196e59:::BB
 gklayout.dll:0x196e59 mov_ld [8+esp] -> eax 
 gklayout.dll:0x196e5d cmp [0+eax] $0xa6cf90e0 
 gklayout.dll:0x196e63 jnz_short $0x1f16e90 


ppt gklayout.dll:0x196e65:::BB
 gklayout.dll:0x196e65 mov_ld [4+eax] -> edx 
 gklayout.dll:0x196e68 cmp edx [32991844+] 
 gklayout.dll:0x196e6e jnz_short $0x1f16e90 


ppt gklayout.dll:0x196e70:::BB
 gklayout.dll:0x196e70 mov_ld [8+eax] -> edx 
 gklayout.dll:0x196e73 cmp edx [32991848+] 
 gklayout.dll:0x196e79 jnz_short $0x1f16e90 


ppt gklayout.dll:0x196e7b:::BB
 gklayout.dll:0x196e7b mov_ld [12+eax] -> edx 
 gklayout.dll:0x196e7e cmp edx [32991852+] 
 gklayout.dll:0x196e84 jnz_short $0x1f16e90 


ppt gklayout.dll:0x196e86:::BB
 gklayout.dll:0x196e86 mov_ld [4+esp] -> eax 
 gklayout.dll:0x196e8a mov_st eax -> [0+ecx] 
 gklayout.dll:0x196e8c xor eax eax -> eax 
 gklayout.dll:0x196e8e jmp_short $0x1f16e9b 


ppt gklayout.dll:0x196e90:::BB
 gklayout.dll:0x196e90 push ecx esp -> esp [0+esp] 
 gklayout.dll:0x196e91 push eax esp -> esp [0+esp] 
 gklayout.dll:0x196e92 push [12+esp] esp -> esp [0+esp] 
 gklayout.dll:0x196e96 call $0x1d8fefe esp -> esp [0+esp] 


ppt gklayout.dll:0x196e9b:::BB
 gklayout.dll:0x196e9b ret $0x000c esp [0+esp] -> esp 


ppt gklayout.dll:0x19165c:::BB
 gklayout.dll:0x19165c push esi esp -> esp [0+esp] 
 gklayout.dll:0x19165d mov_ld [12+esp] -> esi 
 gklayout.dll:0x191661 test esi esi 
 gklayout.dll:0x191663 jnz_short $0x1f1166c 


ppt gklayout.dll:0x191665:::BB
 gklayout.dll:0x191665 mov_imm $0x80004003 -> eax 
 gklayout.dll:0x19166a pop esp [0+esp] -> esi esp 
 gklayout.dll:0x19166b ret esp [0+esp] -> esp 


ppt gklayout.dll:0x19166c:::BB
 gklayout.dll:0x19166c push [8+esp] esp -> esp [0+esp] 
 gklayout.dll:0x191670 push_imm $0x3c esp -> esp [0+esp] 
 gklayout.dll:0x191672 call $0x1d8fe46 esp -> esp [0+esp] 


ppt gklayout.dll:0x191677:::BB
 gklayout.dll:0x191677 pop esp [0+esp] -> ecx esp 
 gklayout.dll:0x191678 test eax eax 
 gklayout.dll:0x19167a pop esp [0+esp] -> ecx esp 
 gklayout.dll:0x19167b jz_short $0x1f11686 


ppt gklayout.dll:0x19167d:::BB
 gklayout.dll:0x19167d mov_ld eax -> ecx 
 gklayout.dll:0x19167f call $0x1f11752 esp -> esp [0+esp] 


ppt gklayout.dll:0x191684:::BB
 gklayout.dll:0x191684 jmp_short $0x1f11688 


ppt gklayout.dll:0x191686:::BB
 gklayout.dll:0x191686 xor eax eax -> eax 


ppt gklayout.dll:0x191688:::BB
 gklayout.dll:0x191688 test eax eax 
 gklayout.dll:0x19168a jnz_short $0x1f11693 


ppt gklayout.dll:0x19168c:::BB
 gklayout.dll:0x19168c mov_imm $0x8007000e -> eax 
 gklayout.dll:0x191691 pop esp [0+esp] -> esi esp 
 gklayout.dll:0x191692 ret esp [0+esp] -> esp 


ppt gklayout.dll:0x191693:::BB
 gklayout.dll:0x191693 mov_st eax -> [0+esi] 
 gklayout.dll:0x191695 xor eax eax -> eax 
 gklayout.dll:0x191697 pop esp [0+esp] -> esi esp 
 gklayout.dll:0x191698 ret esp [0+esp] -> esp 


ppt gklayout.dll:0x191752:::BB
 gklayout.dll:0x191752 push ebp esp -> esp [0+esp] 
 gklayout.dll:0x191753 mov_ld esp -> ebp 
 gklayout.dll:0x191755 sub $0x18 esp -> esp 
 gklayout.dll:0x191758 push edi esp -> esp [0+esp] 
 gklayout.dll:0x191759 mov_ld ecx -> edi 
 gklayout.dll:0x19175b call $0x1d8fe84 esp -> esp [0+esp] 


ppt gklayout.dll:0x191760:::BB
 gklayout.dll:0x191760 mov_st $0x01f76908 -> [0+edi] 
 gklayout.dll:0x191766 cmp [33179596+] $0x00 
 gklayout.dll:0x19176d jnz_short $0x1f117e5 


ppt gklayout.dll:0x19176f:::BB
 gklayout.dll:0x19176f xor eax eax -> eax 
 gklayout.dll:0x191771 push esi esp -> esp [0+esp] 
 gklayout.dll:0x191772 push eax esp -> esp [0+esp] 
 gklayout.dll:0x191773 lea [-16+ebp] -> ecx 
 gklayout.dll:0x191776 mov_st $0x01f52ed8 -> [-24+ebp] 
 gklayout.dll:0x19177d mov_st $0x01f8b798 -> [-20+ebp] 
 gklayout.dll:0x191784 call $0x1d82e49 esp -> esp [0+esp] 


ppt gklayout.dll:0x191789:::BB
 gklayout.dll:0x191789 mov_ld [32839924+] -> eax 
 gklayout.dll:0x19178e and $0x00 [-12+ebp] -> [-12+ebp] 
 gklayout.dll:0x191792 and $0x00 [-4+ebp] -> [-4+ebp] 
 gklayout.dll:0x191796 mov_st eax -> [-24+ebp] 
 gklayout.dll:0x191799 lea [-24+ebp] -> eax 
 gklayout.dll:0x19179c push_imm $0x01f530f0 esp -> esp [0+esp] 
 gklayout.dll:0x1917a1 push eax esp -> esp [0+esp] 
 gklayout.dll:0x1917a2 lea [-4+ebp] -> ecx 
 gklayout.dll:0x1917a5 call_ind [32840044+] esp -> esp [0+esp] 


ppt gklayout.dll:0x1917ab:::BB
 gklayout.dll:0x1917ab mov_ld [32840048+] -> esi 
 gklayout.dll:0x1917b1 lea [-16+ebp] -> ecx 
 gklayout.dll:0x1917b4 call_ind esi esp -> esp [0+esp] 


ppt gklayout.dll:0x1917b6:::BB
 gklayout.dll:0x1917b6 mov_ld [-4+ebp] -> eax 
 gklayout.dll:0x1917b9 test eax eax 
 gklayout.dll:0x1917bb jz_short $0x1f117d8 


ppt gklayout.dll:0x1917bd:::BB
 gklayout.dll:0x1917bd and $0x00 [-8+ebp] -> [-8+ebp] 
 gklayout.dll:0x1917c1 mov_ld [0+eax] -> ecx 
 gklayout.dll:0x1917c3 lea [-8+ebp] -> edx 
 gklayout.dll:0x1917c6 push edx esp -> esp [0+esp] 
 gklayout.dll:0x1917c7 push_imm $0x01f9dab8 esp -> esp [0+esp] 
 gklayout.dll:0x1917cc push eax esp -> esp [0+esp] 
 gklayout.dll:0x1917cd call_ind [20+ecx] esp -> esp [0+esp] 


ppt gklayout.dll:0x1917d0:::BB
 gklayout.dll:0x1917d0 mov_ld [-8+ebp] -> al 
 gklayout.dll:0x1917d3 mov_st al -> [33151664+] 


ppt gklayout.dll:0x1917d8:::BB
 gklayout.dll:0x1917d8 lea [-4+ebp] -> ecx 
 gklayout.dll:0x1917db mov_st $0x01 -> [33179596+] 
 gklayout.dll:0x1917e2 call_ind esi esp -> esp [0+esp] 


ppt gklayout.dll:0x1917e4:::BB
 gklayout.dll:0x1917e4 pop esp [0+esp] -> esi esp 


ppt gklayout.dll:0x1917e5:::BB
 gklayout.dll:0x1917e5 mov_ld edi -> eax 
 gklayout.dll:0x1917e7 pop esp [0+esp] -> edi esp 
 gklayout.dll:0x1917e8 leave ebp esp [0+esp] -> esp ebp 
 gklayout.dll:0x1917e9 ret esp [0+esp] -> esp 


ppt gklayout.dll:0x196d2a:::BB
 gklayout.dll:0x196d2a mov_ld [33173704+] -> eax 
 gklayout.dll:0x196d2f ret esp [0+esp] -> esp 


ppt gklayout.dll:0x197068:::BB
 gklayout.dll:0x197068 push ebp esp -> esp [0+esp] 
 gklayout.dll:0x197069 mov_ld esp -> ebp 
 gklayout.dll:0x19706b cmp [20+ebp] $0x00 
 gklayout.dll:0x19706f jz_short $0x1f17078 


ppt gklayout.dll:0x197071:::BB
 gklayout.dll:0x197071 mov_imm $0x80070057 -> eax 
 gklayout.dll:0x197076 jmp_short $0x1f1709f 


ppt gklayout.dll:0x197078:::BB
 gklayout.dll:0x197078 cmp [24+ebp] $0x00 
 gklayout.dll:0x19707c jz_short $0x1f1709d 


ppt gklayout.dll:0x19707e:::BB
 gklayout.dll:0x19707e push esi esp -> esp [0+esp] 
 gklayout.dll:0x19707f mov_ld [8+ebp] -> esi 
 gklayout.dll:0x197082 push [24+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x197085 lea [48+esi] -> ecx 
 gklayout.dll:0x197088 push esi esp -> esp [0+esp] 
 gklayout.dll:0x197089 call $0x1dfe889 esp -> esp [0+esp] 


ppt gklayout.dll:0x19708e:::BB
 gklayout.dll:0x19708e mov_ld [0+esi] -> eax 
 gklayout.dll:0x197090 push_imm $0x00 esp -> esp [0+esp] 
 gklayout.dll:0x197092 push [16+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x197095 push esi esp -> esp [0+esp] 
 gklayout.dll:0x197096 call_ind [240+eax] esp -> esp [0+esp] 


ppt gklayout.dll:0x19709c:::BB
 gklayout.dll:0x19709c pop esp [0+esp] -> esi esp 


ppt gklayout.dll:0x19709d:::BB
 gklayout.dll:0x19709d xor eax eax -> eax 


ppt gklayout.dll:0x19709f:::BB
 gklayout.dll:0x19709f pop esp [0+esp] -> ebp esp 
 gklayout.dll:0x1970a0 ret $0x0014 esp [0+esp] -> esp 


ppt gklayout.dll:0x19614c:::BB
 gklayout.dll:0x19614c push ebp esp -> esp [0+esp] 
 gklayout.dll:0x19614d mov_ld esp -> ebp 
 gklayout.dll:0x19614f sub $0x00000324 esp -> esp 
 gklayout.dll:0x196155 push esi esp -> esp [0+esp] 
 gklayout.dll:0x196156 push edi esp -> esp [0+esp] 
 gklayout.dll:0x196157 mov_ld [20+ebp] -> edi 
 gklayout.dll:0x19615a xor esi esi -> esi 
 gklayout.dll:0x19615c cmp [36+edi] esi 
 gklayout.dll:0x19615f jnz_short $0x1f16194 


ppt gklayout.dll:0x196161:::BB
 gklayout.dll:0x196161 mov_ld [16+ebp] -> eax 
 gklayout.dll:0x196164 cmp [73+eax] $0x00 
 gklayout.dll:0x196168 mov_st esi -> [0+eax] 
 gklayout.dll:0x19616a mov_st esi -> [4+eax] 
 gklayout.dll:0x19616d mov_st esi -> [8+eax] 
 gklayout.dll:0x196170 mov_st esi -> [12+eax] 
 gklayout.dll:0x196173 jz_short $0x1f16178 


ppt gklayout.dll:0x196175:::BB
 gklayout.dll:0x196175 mov_st esi -> [16+eax] 


ppt gklayout.dll:0x196178:::BB
 gklayout.dll:0x196178 test [68+eax] $0x02 
 gklayout.dll:0x19617c jz_short $0x1f1618d 


ppt gklayout.dll:0x19617e:::BB
 gklayout.dll:0x19617e mov_st esi -> [28+eax] 
 gklayout.dll:0x196181 mov_st esi -> [24+eax] 
 gklayout.dll:0x196184 mov_st esi -> [40+eax] 
 gklayout.dll:0x196187 mov_st esi -> [36+eax] 
 gklayout.dll:0x19618a mov_st esi -> [32+eax] 


ppt gklayout.dll:0x19618d:::BB
 gklayout.dll:0x19618d xor eax eax -> eax 
 gklayout.dll:0x19618f jmp $0x1f16703 


ppt gklayout.dll:0x196194:::BB
 gklayout.dll:0x196194 push ebx esp -> esp [0+esp] 
 gklayout.dll:0x196195 mov_ld [8+ebp] -> ebx 
 gklayout.dll:0x196198 lea [-32+ebp] -> ecx 
 gklayout.dll:0x19619b mov_st esi -> [-12+ebp] 
 gklayout.dll:0x19619e mov_ld [0+ebx] -> eax 
 gklayout.dll:0x1961a0 push ecx esp -> esp [0+esp] 
 gklayout.dll:0x1961a1 push ebx esp -> esp [0+esp] 
 gklayout.dll:0x1961a2 call_ind [132+eax] esp -> esp [0+esp] 


ppt gklayout.dll:0x1961a8:::BB
 gklayout.dll:0x1961a8 mov_ld [-32+ebp] -> eax 
 gklayout.dll:0x1961ab cmp eax esi 
 gklayout.dll:0x1961ad jz_short $0x1f161c1 


ppt gklayout.dll:0x1961af:::BB
 gklayout.dll:0x1961af mov_ld [48+eax] -> ecx 
 gklayout.dll:0x1961b2 add [44+eax] ecx -> ecx 
 gklayout.dll:0x1961b5 cmp ecx [44+ebx] 
 gklayout.dll:0x1961b8 mov_st ecx -> [-12+ebp] 
 gklayout.dll:0x1961bb jz_short $0x1f161c1 


ppt gklayout.dll:0x1961bd:::BB
 gklayout.dll:0x1961bd and $0xbf [39+ebx] -> [39+ebx] 


ppt gklayout.dll:0x1961c1:::BB
 gklayout.dll:0x1961c1 push [24+ebx] esp -> esp [0+esp] 
 gklayout.dll:0x1961c4 mov_ld [36+edi] -> eax 
 gklayout.dll:0x1961c7 lea [-196+ebp] -> ecx 
 gklayout.dll:0x1961cd mov_st eax -> [-16+ebp] 
 gklayout.dll:0x1961d0 push [24+edi] esp -> esp [0+esp] 
 gklayout.dll:0x1961d3 push [12+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x1961d6 call $0x1f11a97 esp -> esp [0+esp] 


ppt gklayout.dll:0x1961db:::BB
 gklayout.dll:0x1961db cmp [48+ebx] esi 
 gklayout.dll:0x1961de jle_short $0x1f161ec 


ppt gklayout.dll:0x1961e0:::BB
 gklayout.dll:0x1961e0 test [38+ebx] $0x02 
 gklayout.dll:0x1961e4 jz_short $0x1f161ec 


ppt gklayout.dll:0x1961e6:::BB
 gklayout.dll:0x1961e6 mov_ld [44+ebx] -> eax 
 gklayout.dll:0x1961e9 mov_st eax -> [-12+ebp] 


ppt gklayout.dll:0x1961ec:::BB
 gklayout.dll:0x1961ec mov_ld [12+ebp] -> eax 
 gklayout.dll:0x1961ef lea [-40+ebp] -> edx 
 gklayout.dll:0x1961f2 push edx esp -> esp [0+esp] 
 gklayout.dll:0x1961f3 push eax esp -> esp [0+esp] 
 gklayout.dll:0x1961f4 mov_ld [0+eax] -> ecx 
 gklayout.dll:0x1961f6 call_ind [96+ecx] esp -> esp [0+esp] 


ppt gklayout.dll:0x1961f9:::BB
 gklayout.dll:0x1961f9 cmp [-40+ebp] esi 
 gklayout.dll:0x1961fc jz_short $0x1f1625b 


ppt gklayout.dll:0x1961fe:::BB
 gklayout.dll:0x1961fe mov_ld [24+edi] -> eax 
 gklayout.dll:0x196201 lea [-4+ebp] -> edx 
 gklayout.dll:0x196204 mov_st esi -> [8+ebp] 
 gklayout.dll:0x196207 mov_st esi -> [-4+ebp] 
 gklayout.dll:0x19620a mov_ld [0+eax] -> ecx 
 gklayout.dll:0x19620c push edx esp -> esp [0+esp] 
 gklayout.dll:0x19620d push eax esp -> esp [0+esp] 
 gklayout.dll:0x19620e call_ind [44+ecx] esp -> esp [0+esp] 


ppt gklayout.dll:0x196211:::BB
 gklayout.dll:0x196211 lea [8+ebp] -> ecx 
 gklayout.dll:0x196214 mov_ld [0+ebx] -> eax 
 gklayout.dll:0x196216 push_imm $0x04 esp -> esp [0+esp] 
 gklayout.dll:0x196218 push ecx esp -> esp [0+esp] 
 gklayout.dll:0x196219 push [33173672+] esp -> esp [0+esp] 
 gklayout.dll:0x19621f push [12+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196222 push ebx esp -> esp [0+esp] 
 gklayout.dll:0x196223 call_ind [268+eax] esp -> esp [0+esp] 


ppt gklayout.dll:0x196229:::BB
 gklayout.dll:0x196229 cmp [8+ebp] $0x0d 
 gklayout.dll:0x19622d jnz_short $0x1f16239 


ppt gklayout.dll:0x19622f:::BB
 gklayout.dll:0x19622f mov_ld [-4+ebp] -> eax 
 gklayout.dll:0x196232 and $0x08 eax -> eax 
 gklayout.dll:0x196235 cmp al $0x08 
 gklayout.dll:0x196237 jz_short $0x1f16249 


ppt gklayout.dll:0x196239:::BB
 gklayout.dll:0x196239 cmp [8+ebp] $0x01 
 gklayout.dll:0x19623d jnz_short $0x1f1625b 


ppt gklayout.dll:0x19623f:::BB
 gklayout.dll:0x19623f mov_ld [-4+ebp] -> eax 
 gklayout.dll:0x196242 and $0x04 eax -> eax 
 gklayout.dll:0x196245 cmp al $0x04 
 gklayout.dll:0x196247 jnz_short $0x1f1625b 


ppt gklayout.dll:0x196249:::BB
 gklayout.dll:0x196249 mov_ld [12+ebp] -> eax 
 gklayout.dll:0x19624c mov_ld [92+eax] -> ecx 
 gklayout.dll:0x19624f and $0xdf ch -> ch 
 gklayout.dll:0x196252 xor $0x00002000 ecx -> ecx 
 gklayout.dll:0x196258 mov_st ecx -> [92+eax] 


ppt gklayout.dll:0x19625b:::BB
 gklayout.dll:0x19625b mov_ld [39+ebx] -> al 
 gklayout.dll:0x19625e and $0xe0 [39+ebx] -> [39+ebx] 
 gklayout.dll:0x196262 and $0x01 eax -> eax 
 gklayout.dll:0x196265 test [174+edi] $0x04 
 gklayout.dll:0x19626c mov_st eax -> [-52+ebp] 
 gklayout.dll:0x19626f mov_ld [36+ebx] -> eax 
 gklayout.dll:0x196272 jz_short $0x1f1628e 


ppt gklayout.dll:0x196274:::BB
 gklayout.dll:0x196274 mov_imm $0x80000000 -> ecx 
 gklayout.dll:0x196279 test ecx eax 
 gklayout.dll:0x19627b jnz_short $0x1f162a1 


ppt gklayout.dll:0x19627d:::BB
 gklayout.dll:0x19627d push ebx esp -> esp [0+esp] 
 gklayout.dll:0x19627e or ecx eax -> eax 
 gklayout.dll:0x196280 push [12+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196283 mov_st eax -> [36+ebx] 
 gklayout.dll:0x196286 call $0x1f11091 esp -> esp [0+esp] 


ppt gklayout.dll:0x19628b:::BB
 gklayout.dll:0x19628b pop esp [0+esp] -> ecx esp 
 gklayout.dll:0x19628c jmp_short $0x1f162a0 


ppt gklayout.dll:0x19628e:::BB
 gklayout.dll:0x19628e test eax eax 
 gklayout.dll:0x196290 jns_short $0x1f162a1 


ppt gklayout.dll:0x196292:::BB
 gklayout.dll:0x196292 and $0x7fffffff eax -> eax 
 gklayout.dll:0x196297 push ebx esp -> esp [0+esp] 
 gklayout.dll:0x196298 mov_st eax -> [36+ebx] 
 gklayout.dll:0x19629b call $0x1f110dc esp -> esp [0+esp] 


ppt gklayout.dll:0x1962a0:::BB
 gklayout.dll:0x1962a0 pop esp [0+esp] -> ecx esp 


ppt gklayout.dll:0x1962a1:::BB
 gklayout.dll:0x1962a1 mov_ld [-192+ebp] -> eax 
 gklayout.dll:0x1962a7 mov_ld [2+eax] -> al 
 gklayout.dll:0x1962aa test al al 
 gklayout.dll:0x1962ac jz_short $0x1f162ba 


ppt gklayout.dll:0x1962ae:::BB
 gklayout.dll:0x1962ae cmp al $0x03 
 gklayout.dll:0x1962b0 jz_short $0x1f162ba 


ppt gklayout.dll:0x1962b2:::BB
 gklayout.dll:0x1962b2 xor eax eax -> eax 
 gklayout.dll:0x1962b4 mov_st esi -> [8+ebp] 
 gklayout.dll:0x1962b7 inc eax -> eax 
 gklayout.dll:0x1962b8 jmp_short $0x1f162c0 


ppt gklayout.dll:0x1962ba:::BB
 gklayout.dll:0x1962ba xor eax eax -> eax 
 gklayout.dll:0x1962bc inc eax -> eax 
 gklayout.dll:0x1962bd mov_st eax -> [8+ebp] 


ppt gklayout.dll:0x1962c0:::BB
 gklayout.dll:0x1962c0 cmp [-136+ebp] esi 
 gklayout.dll:0x1962c6 mov_st esi -> [-24+ebp] 
 gklayout.dll:0x1962c9 jnz_short $0x1f162d6 


ppt gklayout.dll:0x1962cb:::BB
 gklayout.dll:0x1962cb mov_ld [-16+ebp] -> ecx 
 gklayout.dll:0x1962ce test [0+ecx] $0x01 
 gklayout.dll:0x1962d1 jz_short $0x1f162d6 


ppt gklayout.dll:0x1962d3:::BB
 gklayout.dll:0x1962d3 mov_st eax -> [-24+ebp] 


ppt gklayout.dll:0x1962d6:::BB
 gklayout.dll:0x1962d6 mov_ld [16+edi] -> ecx 
 gklayout.dll:0x1962d9 mov_st ecx -> [-36+ebp] 
 gklayout.dll:0x1962dc mov_ld [20+ebx] -> ecx 
 gklayout.dll:0x1962df mov_ld [4+ecx] -> ecx 
 gklayout.dll:0x1962e2 cmp ecx esi 
 gklayout.dll:0x1962e4 jnz_short $0x1f162f0 


ppt gklayout.dll:0x1962e6:::BB
 gklayout.dll:0x1962e6 mov_imm $0x80004005 -> esi 
 gklayout.dll:0x1962eb jmp $0x1f166f5 


ppt gklayout.dll:0x1962f0:::BB
 gklayout.dll:0x1962f0 cmp [-168+ebp] esi 
 gklayout.dll:0x1962f6 jnz_short $0x1f16313 


ppt gklayout.dll:0x1962f8:::BB
 gklayout.dll:0x1962f8 cmp [-164+ebp] esi 
 gklayout.dll:0x1962fe jnz_short $0x1f16313 


ppt gklayout.dll:0x196300:::BB
 gklayout.dll:0x196300 cmp [-160+ebp] esi 
 gklayout.dll:0x196306 jnz_short $0x1f16313 


ppt gklayout.dll:0x196308:::BB
 gklayout.dll:0x196308 cmp [-140+ebp] esi 
 gklayout.dll:0x19630e mov_st esi -> [-4+ebp] 
 gklayout.dll:0x196311 jz_short $0x1f16316 


ppt gklayout.dll:0x196313:::BB
 gklayout.dll:0x196313 mov_st eax -> [-4+ebp] 


ppt gklayout.dll:0x196316:::BB
 gklayout.dll:0x196316 mov_ld [0+ecx] -> eax 
 gklayout.dll:0x196318 call_ind [64+eax] esp -> esp [0+esp] 


ppt gklayout.dll:0x19631b:::BB
 gklayout.dll:0x19631b push [12+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x19631e lea [-804+ebp] -> ecx 
 gklayout.dll:0x196324 mov_st eax -> [-48+ebp] 
 gklayout.dll:0x196327 push esi esp -> esp [0+esp] 
 gklayout.dll:0x196328 push eax esp -> esp [0+esp] 
 gklayout.dll:0x196329 call $0x1d83fd2 esp -> esp [0+esp] 


ppt gklayout.dll:0x19632e:::BB
 gklayout.dll:0x19632e xor eax eax -> eax 
 gklayout.dll:0x196330 cmp [-168+ebp] esi 
 gklayout.dll:0x196336 lea [-804+ebp] -> ecx 
 gklayout.dll:0x19633c setnz -> al 
 gklayout.dll:0x19633f push eax esp -> esp [0+esp] 
 gklayout.dll:0x196340 push [-4+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196343 push [-12+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196346 push [20+ebx] esp -> esp [0+esp] 
 gklayout.dll:0x196349 push ebx esp -> esp [0+esp] 
 gklayout.dll:0x19634a call $0x1d84028 esp -> esp [0+esp] 


ppt gklayout.dll:0x19634f:::BB
 gklayout.dll:0x19634f cmp eax esi 
 gklayout.dll:0x196351 jz_short $0x1f1635a 


ppt gklayout.dll:0x196353:::BB
 gklayout.dll:0x196353 mov_ld eax -> esi 
 gklayout.dll:0x196355 jmp $0x1f166ea 


ppt gklayout.dll:0x19635a:::BB
 gklayout.dll:0x19635a mov_ld [-16+ebp] -> eax 
 gklayout.dll:0x19635d cmp [68+eax] esi 
 gklayout.dll:0x196360 jnz_short $0x1f16374 


ppt gklayout.dll:0x196362:::BB
 gklayout.dll:0x196362 mov_ld [-32+ebp] -> ecx 
 gklayout.dll:0x196365 cmp ecx esi 
 gklayout.dll:0x196367 jz_short $0x1f1636f 


ppt gklayout.dll:0x196369:::BB
 gklayout.dll:0x196369 test [39+ecx] $0x08 
 gklayout.dll:0x19636d jnz_short $0x1f16374 


ppt gklayout.dll:0x19636f:::BB
 gklayout.dll:0x19636f mov_st esi -> [-28+ebp] 
 gklayout.dll:0x196372 jmp_short $0x1f1637f 


ppt gklayout.dll:0x196374:::BB
 gklayout.dll:0x196374 or $0x04 [39+ebx] -> [39+ebx] 
 gklayout.dll:0x196378 mov_st $0x00000001 -> [-28+ebp] 


ppt gklayout.dll:0x19637f:::BB
 gklayout.dll:0x19637f and $0xf7 [39+ebx] -> [39+ebx] 
 gklayout.dll:0x196383 mov_ld [48+eax] -> eax 
 gklayout.dll:0x196386 mov_ld [52+ebx] -> edx 
 gklayout.dll:0x196389 mov_ld [36+ebx] -> ecx 
 gklayout.dll:0x19638c mov_st eax -> [52+ebx] 
 gklayout.dll:0x19638f cmp [8+edi] $0x02 
 gklayout.dll:0x196393 mov_ld [16+ebp] -> edi 
 gklayout.dll:0x196396 mov_st edx -> [-44+ebp] 
 gklayout.dll:0x196399 mov_st $0x00000001 -> [-8+ebp] 
 gklayout.dll:0x1963a0 jnz_short $0x1f16412 


ppt gklayout.dll:0x1963a2:::BB
 gklayout.dll:0x1963a2 test ch $0x04 
 gklayout.dll:0x1963a5 jnz_short $0x1f16412 


ppt gklayout.dll:0x1963a7:::BB
 gklayout.dll:0x1963a7 mov_ld [12+ebx] -> edx 
 gklayout.dll:0x1963aa test ecx $0x20000000 
 gklayout.dll:0x1963b0 mov_st edx -> [-4+ebp] 
 gklayout.dll:0x1963b3 jz_short $0x1f163ce 


ppt gklayout.dll:0x1963b5:::BB
 gklayout.dll:0x1963b5 mov_ld [-160+ebp] -> edx 
 gklayout.dll:0x1963bb mov_ld [-148+ebp] -> esi 
 gklayout.dll:0x1963c1 add edx esi -> esi 
 gklayout.dll:0x1963c3 add [-164+ebp] esi -> esi 
 gklayout.dll:0x1963c9 add esi [-4+ebp] -> [-4+ebp] 
 gklayout.dll:0x1963cc xor esi esi -> esi 


ppt gklayout.dll:0x1963ce:::BB
 gklayout.dll:0x1963ce cmp [40+ebx] esi 
 gklayout.dll:0x1963d1 jnz_short $0x1f16412 


ppt gklayout.dll:0x1963d3:::BB
 gklayout.dll:0x1963d3 test ecx $0x40000000 
 gklayout.dll:0x1963d9 jz_short $0x1f16412 


ppt gklayout.dll:0x1963db:::BB
 gklayout.dll:0x1963db cmp [73+edi] $0x00 
 gklayout.dll:0x1963df jnz_short $0x1f16412 


ppt gklayout.dll:0x1963e1:::BB
 gklayout.dll:0x1963e1 mov_ld [-52+ebp] -> edx 
 gklayout.dll:0x1963e4 cmp edx [-24+ebp] 
 gklayout.dll:0x1963e7 jnz_short $0x1f16412 


ppt gklayout.dll:0x1963e9:::BB
 gklayout.dll:0x1963e9 xor edx edx -> edx 
 gklayout.dll:0x1963eb cmp [8+ebp] edx 
 gklayout.dll:0x1963ee jz_short $0x1f163fa 


ppt gklayout.dll:0x1963f0:::BB
 gklayout.dll:0x1963f0 mov_ld [-4+ebp] -> eax 
 gklayout.dll:0x1963f3 cmp [-36+ebp] eax 
 gklayout.dll:0x1963f6 jnl_short $0x1f163ff 


ppt gklayout.dll:0x1963f8:::BB
 gklayout.dll:0x1963f8 jmp_short $0x1f16412 


ppt gklayout.dll:0x1963fa:::BB
 gklayout.dll:0x1963fa cmp [-44+ebp] eax 
 gklayout.dll:0x1963fd jnz_short $0x1f16412 


ppt gklayout.dll:0x1963ff:::BB
 gklayout.dll:0x1963ff test ecx $0x00020000 
 gklayout.dll:0x196405 jnz_short $0x1f16412 


ppt gklayout.dll:0x196407:::BB
 gklayout.dll:0x196407 cmp [-140+ebp] edx 
 gklayout.dll:0x19640d jnz_short $0x1f16412 


ppt gklayout.dll:0x19640f:::BB
 gklayout.dll:0x19640f mov_st edx -> [-8+ebp] 


ppt gklayout.dll:0x196412:::BB
 gklayout.dll:0x196412 mov_ld [-16+ebp] -> ecx 
 gklayout.dll:0x196415 push_imm $0x00 esp -> esp [0+esp] 
 gklayout.dll:0x196417 ud2b [73+edi] -> eax 
 gklayout.dll:0x19641b ud2b [0+ecx] -> esi 
 gklayout.dll:0x19641e sar $0x03 esi -> esi 
 gklayout.dll:0x196421 push eax esp -> esp [0+esp] 
 gklayout.dll:0x196422 and $0x01 esi -> esi 
 gklayout.dll:0x196425 call $0x1da2e97 esp -> esp [0+esp] 


ppt gklayout.dll:0x19642a:::BB
 gklayout.dll:0x19642a push eax esp -> esp [0+esp] 
 gklayout.dll:0x19642b push esi esp -> esp [0+esp] 
 gklayout.dll:0x19642c push [-28+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x19642f lea [-80+ebp] -> ecx 
 gklayout.dll:0x196432 push [-8+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196435 push [-24+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196438 push [8+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x19643b push [-12+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x19643e call $0x1f16709 esp -> esp [0+esp] 


ppt gklayout.dll:0x196443:::BB
 gklayout.dll:0x196443 mov_ld [-196+ebp] -> eax 
 gklayout.dll:0x196449 cmp [40+eax] $0x00 
 gklayout.dll:0x19644d jz_short $0x1f1647e 


ppt gklayout.dll:0x19644f:::BB
 gklayout.dll:0x19644f lea [-80+ebp] -> eax 
 gklayout.dll:0x196452 mov_ld ebx -> ecx 
 gklayout.dll:0x196454 push eax esp -> esp [0+esp] 
 gklayout.dll:0x196455 lea [-196+ebp] -> eax 
 gklayout.dll:0x19645b push eax esp -> esp [0+esp] 
 gklayout.dll:0x19645c lea [-804+ebp] -> eax 
 gklayout.dll:0x196462 push [-48+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196465 push eax esp -> esp [0+esp] 
 gklayout.dll:0x196466 push [20+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x196469 push [12+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x19646c call $0x1f1579b esp -> esp [0+esp] 


ppt gklayout.dll:0x196471:::BB
 gklayout.dll:0x196471 mov_ld [24+ebp] -> ecx 
 gklayout.dll:0x196474 mov_ld [-64+ebp] -> edx 
 gklayout.dll:0x196477 mov_st eax -> [0+ecx] 
 gklayout.dll:0x196479 mov_ld [-68+ebp] -> ecx 
 gklayout.dll:0x19647c jmp_short $0x1f16490 


ppt gklayout.dll:0x19647e:::BB
 gklayout.dll:0x19647e mov_ld [24+ebp] -> eax 
 gklayout.dll:0x196481 xor edx edx -> edx 
 gklayout.dll:0x196483 xor ecx ecx -> ecx 
 gklayout.dll:0x196485 mov_st edx -> [-80+ebp] 
 gklayout.dll:0x196488 mov_st ecx -> [-68+ebp] 
 gklayout.dll:0x19648b mov_st edx -> [-64+ebp] 
 gklayout.dll:0x19648e mov_st edx -> [0+eax] 


ppt gklayout.dll:0x196490:::BB
 gklayout.dll:0x196490 cmp [-53+ebp] $0x00 
 gklayout.dll:0x196494 jz_short $0x1f1649c 


ppt gklayout.dll:0x196496:::BB
 gklayout.dll:0x196496 or $0x20 [39+ebx] -> [39+ebx] 
 gklayout.dll:0x19649a jmp_short $0x1f164a0 


ppt gklayout.dll:0x19649c:::BB
 gklayout.dll:0x19649c and $0xdf [39+ebx] -> [39+ebx] 


ppt gklayout.dll:0x1964a0:::BB
 gklayout.dll:0x1964a0 test [-503+ebp] $0x02 
 gklayout.dll:0x1964a7 jz_short $0x1f164ad 


ppt gklayout.dll:0x1964a9:::BB
 gklayout.dll:0x1964a9 or $0x02 [39+ebx] -> [39+ebx] 


ppt gklayout.dll:0x1964ad:::BB
 gklayout.dll:0x1964ad mov_ld [-80+ebp] -> esi 
 gklayout.dll:0x1964b0 xor eax eax -> eax 
 gklayout.dll:0x1964b2 cmp esi eax 
 gklayout.dll:0x1964b4 mov_st esi -> [0+edi] 
 gklayout.dll:0x1964b6 jnz_short $0x1f164cb 


ppt gklayout.dll:0x1964b8:::BB
 gklayout.dll:0x1964b8 cmp [-136+ebp] eax 
 gklayout.dll:0x1964be jnz_short $0x1f164cb 


ppt gklayout.dll:0x1964c0:::BB
 gklayout.dll:0x1964c0 mov_st eax -> [4+edi] 
 gklayout.dll:0x1964c3 mov_st eax -> [8+edi] 
 gklayout.dll:0x1964c6 mov_st eax -> [12+edi] 
 gklayout.dll:0x1964c9 jmp_short $0x1f164d6 


ppt gklayout.dll:0x1964cb:::BB
 gklayout.dll:0x1964cb mov_st edx -> [12+edi] 
 gklayout.dll:0x1964ce add ecx edx -> edx 
 gklayout.dll:0x1964d0 mov_st ecx -> [8+edi] 
 gklayout.dll:0x1964d3 mov_st edx -> [4+edi] 


ppt gklayout.dll:0x1964d6:::BB
 gklayout.dll:0x1964d6 mov_ld [8+edi] -> ecx 
 gklayout.dll:0x1964d9 cmp [8+ebp] eax 
 gklayout.dll:0x1964dc mov_st ecx -> [56+ebx] 
 gklayout.dll:0x1964df jnz_short $0x1f164e4 


ppt gklayout.dll:0x1964e1:::BB
 gklayout.dll:0x1964e1 mov_st esi -> [-72+ebp] 


ppt gklayout.dll:0x1964e4:::BB
 gklayout.dll:0x1964e4 cmp [73+edi] $0x00 
 gklayout.dll:0x1964e8 jz_short $0x1f164f0 


ppt gklayout.dll:0x1964ea:::BB
 gklayout.dll:0x1964ea mov_ld [-72+ebp] -> ecx 
 gklayout.dll:0x1964ed mov_st ecx -> [16+edi] 


ppt gklayout.dll:0x1964f0:::BB
 gklayout.dll:0x1964f0 mov_ld [-12+ebp] -> ecx 
 gklayout.dll:0x1964f3 mov_ld [-76+ebp] -> edx 
 gklayout.dll:0x1964f6 sub ecx edx -> edx 
 gklayout.dll:0x1964f8 cmp [-140+ebp] eax 
 gklayout.dll:0x1964fe mov_st ecx -> [44+ebx] 
 gklayout.dll:0x196501 mov_st edx -> [48+ebx] 
 gklayout.dll:0x196504 jz_short $0x1f1652a 


ppt gklayout.dll:0x196506:::BB
 gklayout.dll:0x196506 lea [8+ebp] -> ecx 
 gklayout.dll:0x196509 push_imm $0x01 esp -> esp [0+esp] 
 gklayout.dll:0x19650b push ecx esp -> esp [0+esp] 
 gklayout.dll:0x19650c push eax esp -> esp [0+esp] 
 gklayout.dll:0x19650d push eax esp -> esp [0+esp] 
 gklayout.dll:0x19650e lea [-804+ebp] -> eax 
 gklayout.dll:0x196514 push eax esp -> esp [0+esp] 
 gklayout.dll:0x196515 mov_ld ebx -> ecx 
 gklayout.dll:0x196517 call $0x1f11c2f esp -> esp [0+esp] 


ppt gklayout.dll:0x19651c:::BB
 gklayout.dll:0x19651c mov_ld [8+ebp] -> edx 
 gklayout.dll:0x19651f mov_ld [-16+ebp] -> ecx 
 gklayout.dll:0x196522 sub eax edx -> edx 
 gklayout.dll:0x196524 mov_st eax -> [52+ecx] 
 gklayout.dll:0x196527 mov_st edx -> [56+ecx] 


ppt gklayout.dll:0x19652a:::BB
 gklayout.dll:0x19652a test [68+edi] $0x02 
 gklayout.dll:0x19652e jnz $0x1f16635 


ppt gklayout.dll:0x196534:::BB
 gklayout.dll:0x196534 mov_ld [16+ebp] -> eax 
 gklayout.dll:0x196537 xor esi esi -> esi 
 gklayout.dll:0x196539 push_imm $0x01f55404 esp -> esp [0+esp] 
 gklayout.dll:0x19653e lea [8+ebp] -> ecx 
 gklayout.dll:0x196541 lea [24+eax] -> edi 
 gklayout.dll:0x196544 mov_st $0x8000ffff -> [-8+ebp] 
 gklayout.dll:0x19654b mov_st esi -> [8+ebp] 
 gklayout.dll:0x19654e mov_st esi -> [4+edi] 
 gklayout.dll:0x196551 mov_st esi -> [0+edi] 
 gklayout.dll:0x196553 mov_st esi -> [16+edi] 
 gklayout.dll:0x196556 mov_st esi -> [12+edi] 
 gklayout.dll:0x196559 mov_st esi -> [8+edi] 
 gklayout.dll:0x19655c mov_ld [20+ebx] -> eax 
 gklayout.dll:0x19655f push eax esp -> esp [0+esp] 
 gklayout.dll:0x196560 call_ind [32839876+] esp -> esp [0+esp] 


ppt gklayout.dll:0x196566:::BB
 gklayout.dll:0x196566 cmp [8+ebp] esi 
 gklayout.dll:0x196569 jnz $0x1f1660f 


ppt gklayout.dll:0x19656f:::BB
 gklayout.dll:0x19656f lea [-500+ebp] -> ecx 
 gklayout.dll:0x196575 call $0x1d85b74 esp -> esp [0+esp] 


ppt gklayout.dll:0x19657a:::BB
 gklayout.dll:0x19657a mov_ld [8+ebp] -> eax 
 gklayout.dll:0x19657d lea [-500+ebp] -> edx 
 gklayout.dll:0x196583 push edx esp -> esp [0+esp] 
 gklayout.dll:0x196584 push eax esp -> esp [0+esp] 
 gklayout.dll:0x196585 mov_ld [0+eax] -> ecx 
 gklayout.dll:0x196587 call_ind [112+ecx] esp -> esp [0+esp] 


ppt gklayout.dll:0x19658a:::BB
 gklayout.dll:0x19658a lea [-348+ebp] -> ecx 
 gklayout.dll:0x196590 call $0x1d85b74 esp -> esp [0+esp] 


ppt gklayout.dll:0x196595:::BB
 gklayout.dll:0x196595 push [48+ebx] esp -> esp [0+esp] 
 gklayout.dll:0x196598 lea [-348+ebp] -> eax 
 gklayout.dll:0x19659e lea [-500+ebp] -> ecx 
 gklayout.dll:0x1965a4 push [44+ebx] esp -> esp [0+esp] 
 gklayout.dll:0x1965a7 push eax esp -> esp [0+esp] 
 gklayout.dll:0x1965a8 call_ind [32839468+] esp -> esp [0+esp] 


ppt gklayout.dll:0x1965ae:::BB
 gklayout.dll:0x1965ae push [24+ebx] esp -> esp [0+esp] 
 gklayout.dll:0x1965b1 mov_ld [20+ebp] -> eax 
 gklayout.dll:0x1965b4 push [24+eax] esp -> esp [0+esp] 
 gklayout.dll:0x1965b7 call $0x1d8fdb7 esp -> esp [0+esp] 


ppt gklayout.dll:0x1965bc:::BB
 gklayout.dll:0x1965bc mov_ld [20+ebp] -> eax 
 gklayout.dll:0x1965bf pop esp [0+esp] -> ecx esp 
 gklayout.dll:0x1965c0 pop esp [0+esp] -> ecx esp 
 gklayout.dll:0x1965c1 lea [-100+ebp] -> edx 
 gklayout.dll:0x1965c4 mov_ld [24+eax] -> eax 
 gklayout.dll:0x1965c7 push esi esp -> esp [0+esp] 
 gklayout.dll:0x1965c8 push edx esp -> esp [0+esp] 
 gklayout.dll:0x1965c9 mov_st esi -> [-96+ebp] 
 gklayout.dll:0x1965cc push [48+ebx] esp -> esp [0+esp] 
 gklayout.dll:0x1965cf mov_st esi -> [-100+ebp] 
 gklayout.dll:0x1965d2 mov_st esi -> [-84+ebp] 
 gklayout.dll:0x1965d5 mov_st esi -> [-88+ebp] 
 gklayout.dll:0x1965d8 push [-344+ebp] esp -> esp [0+esp] 
 gklayout.dll:0x1965de mov_st esi -> [-92+ebp] 
 gklayout.dll:0x1965e1 mov_ld [0+eax] -> ecx 
 gklayout.dll:0x1965e3 push eax esp -> esp [0+esp] 
 gklayout.dll:0x1965e4 call_ind [312+ecx] esp -> esp [0+esp] 

