#define move_to_user_mode() \
__asm__ ("movl %%esp,%%eax\n\t" \
	"pushl $0x17\n\t" \	/*usr SS seg selector*/
	"pushl %%eax\n\t" \ /*usr ESP*/
	"pushfl\n\t" \		/*usr eflags*/
	"pushl $0x0f\n\t" \ /*usr CS seg selector*/
	"pushl $1f\n\t" \	/*first usr mode instruction*/
	"iret\n" \			/*pop to usr EIP, CS, eflags, ESP, SS*/
			 \
	/*change to user seg selector: 00010<DataSeg>,1<USR>,11<RPL>*/ \
	"1:\t movl $0x17,%%eax\n\t" \
	"movw %%ax,%%ds\n\t" \
	"movw %%ax,%%es\n\t" \
	"movw %%ax,%%fs\n\t" \
	"movw %%ax,%%gs" \
	:/*output*/\
	:/*input*/\
	:"ax"/*reg*/)

#define sti() __asm__ ("sti"::)
#define cli() __asm__ ("cli"::)
#define nop() __asm__ ("nop"::)

#define iret() __asm__ ("iret"::)

#define _set_gate(gate_addr,type,dpl,addr) \
__asm__ ("movw %%dx,%%ax\n\t" \
	"movw %0,%%dx\n\t" \
	"movl %%eax,%1\n\t" \  /*seg_select , off0-15*/
	"movl %%edx,%2" \						/*off16-31, present,type,dpl...*/
	: \
	: "i" ((short) (0x8000+(dpl<<13)+(type<<8))), \
	"o" (*((char *) (gate_addr))), \   /*seg_select , off0-15*/
	"o" (*(4+(char *) (gate_addr))), \		/*off16-31, present,type,dpl...*/
	"d" ((char *) (addr)),"a" (0x00080000))
  // |_edx,dx,				|_eax,ax |_code seg

#define set_intr_gate(n,addr) \
	_set_gate(&idt[n],14,0,addr)//type=14 : Execute/Read, conforming

#define set_trap_gate(n,addr) \
	_set_gate(&idt[n],15,0,addr) //type=15 : Execute/Read, conforming, accessed

#define set_system_gate(n,addr) \
	_set_gate(&idt[n],15,3,addr)//type=15 : Execute/Read, conforming, accessed

#define _set_seg_desc(gate_addr,type,dpl,base,limit) {\
	*(gate_addr) = ((base) & 0xff000000) | \
		(((base) & 0x00ff0000)>>16) | \
		((limit) & 0xf0000) | \
		((dpl)<<13) | \
		(0x00408000) | \
		((type)<<8); \
	*((gate_addr)+1) = (((base) & 0x0000ffff)<<16) | \
		((limit) & 0x0ffff); }

#define _set_tssldt_desc(n,addr,type) \
__asm__ ("movw $104,%1\n\t" \
	"movw %%ax,%2\n\t" \
	"rorl $16,%%eax\n\t" \
	"movb %%al,%3\n\t" \
	"movb $" type ",%4\n\t" \
	"movb $0x00,%5\n\t" \
	"movb %%ah,%6\n\t" \
	"rorl $16,%%eax" \
	::"a" (addr), "m" (*(n)), "m" (*(n+2)), "m" (*(n+4)), \
	 "m" (*(n+5)), "m" (*(n+6)), "m" (*(n+7)) \
	)
																		  //P, DPL, S, TYPE
#define set_tss_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x89")//1,  00, 0, 1001=Code, Execute-Only, accessed
#define set_ldt_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x82")//1,  00, 0, 0010=Data, Read/Write