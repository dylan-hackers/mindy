
struct machine_state;

extern void call_on_stack(void (*fn)(void *arg), void *arg,
			  void *stack_base, void *stack_end);
extern long save_state(long (*fn)(struct machine_state *state, void *arg),
		       void *arg);
extern void restore_state(struct machine_state *state, long result);
extern void enumerate_stack(void (*fn)(void *ptr, int bytes),
			    struct machine_state *state,
			    void *stack_base, void *stack_end);
