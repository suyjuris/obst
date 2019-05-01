
// Written by Philipp Czerner, 2018. Public Domain.
// See LICENSE.md for license information.

// The README contains some high-level remarks about the code, you might want to take a look.


// Uncomment this to show the force-based layout algorithm. Debug functionality, obviously.
//#define OBST_DBG_SHOW_FORCE_LAYOUT 1


//@Cleanup: Rename opengl to opengl


// Display an error somewhere, somehow. This is not for critical errors where we exit the program,
// just to tell the user that they did something we do not like. ui_error_report is a printf-like
// function.

Array_dyn<u8> ui_error_buf;

template <typename... Args>
void ui_error_report(char const* msg, Args... args) {
    u64 len = snprintf(nullptr, 0, msg, args...);
    ui_error_buf.size = 0;
    array_reserve(&ui_error_buf, len+1);
    snprintf(
        (char*)ui_error_buf.end(),
        ui_error_buf.capacity - ui_error_buf.size,
        msg, args...
    );
    ui_error_buf.size += len+1;

    platform_fmt_store_simple(Text_fmt::PARAGRAPH | Text_fmt::RED, ui_error_buf, Text_fmt::SLOT_ERRORINFO);
}
void ui_error_report(char const* msg) {
    platform_fmt_store_simple(Text_fmt::PARAGRAPH | Text_fmt::RED, msg, Text_fmt::SLOT_ERRORINFO);
}

// These are used for the parser.
namespace errorcodes {

enum errorcode : u8 {
    SUCCESS = 0,
    STRING_EMPTY = 1,
    INVALID_CHARACTER = 2,
    OUT_OF_RANGE_TOO_LOW = 3,
    OUT_OF_RANGE_TOO_HIGH = 4,
    ERROR = 5 
};

}

char const* jup_err_messages[] = {
    /* 0 */ nullptr,
    /* 1 */ "String is empty",
    /* 2 */ "Invalid character",
    /* 3 */ "Out of range (too low)",
    /* 4 */ "Out of range (too high)",
};

// Parse an integer in str into val using base base. Is pretty defensive about what kind of strings
// it accepts.
u8 jup_stoi(Array_t<u8> str, s32* val, s64 base) {
    assert(val);
    assert(2 <= base and base <= 16);

    if (str.size == 0) return errorcodes::STRING_EMPTY;
    
    bool negate = false;
    if (str[0] == '+') {
        str = array_subarray(str, 1, str.size-1);
    } else if (str[0] == '-') {
        negate = true;
        str = array_subarray(str, 1, str.size-1);
    }

    if (str.size == 0) return errorcodes::STRING_EMPTY;
    
    u32 tmp = 0;
    for (char c: str) {
        u32 c_val;
        if ('0' <= c and c <= '9') {
            c_val = c - '0';
        } else if ('a' <= c and c <= 'z') {
            c_val = c - 'a' + 10;
        } else if ('A' <= c and c <= 'Z') {
            c_val = c - 'A' + 10;
        } else {
            return errorcodes::INVALID_CHARACTER;
        }
        if (c_val >= base) {
            return errorcodes::INVALID_CHARACTER;
        }
        
        u32 tmp_old = tmp;
        tmp = tmp * base + c_val;
        if (tmp_old > tmp) {
            return negate ? errorcodes::OUT_OF_RANGE_TOO_LOW
                : errorcodes::OUT_OF_RANGE_TOO_HIGH;
        }
    }
    
    if (negate) {
        tmp = -tmp;
        if (not (tmp >> 31) and tmp) {
            return errorcodes::OUT_OF_RANGE_TOO_LOW;
        }
        *val = (s32)tmp;
        return errorcodes::SUCCESS;
    } else {
        if (tmp >> 31) {
            return errorcodes::OUT_OF_RANGE_TOO_HIGH;
        }
        *val = (s32)tmp;
        return errorcodes::SUCCESS;
    }
}

// Reads a list of integers from str and parses it into into. In this context, list means
// alphanumerical separated by non-alphanumerical.
u8 parse_int_list(Array_dyn<s64>* into, Array_t<u8> str, s64 base) {
    s64 last = 0;
    for (s64 i = 0; i <= str.size; ++i) {
        bool alnum = false;
        if (i < str.size) {
            if ('0' <= str[i] and str[i] <= '9') alnum = true;
            if ('a' <= str[i] and str[i] <= 'z') alnum = true;
            if ('A' <= str[i] and str[i] <= 'Z') alnum = true;
        }
        if (not alnum and last < i) {
            s32 number;
            u8 code = jup_stoi(array_subarray(str, last, i), &number, base);
            if (code) return code;
            last = i+1;
            array_push_back(into, (s64)number);
        } else if (not alnum) {
            last = i+1;
        }
    }
    return 0;
}

// This represents the nodes of the graph. (In the graph sense, so no positions and such.)
struct Bdd {
    // Which flags we allow
    enum Bdd_constants: u8 {
        NONE = 0,
        TEMPORARY = 1,    // These nodes are created during the execution of a stepwise algorithm to illustrate the current state to the puny humans watching it. They are not subject to deduplication and are either deleted or finalised using bdd_finalize.
        CURRENT = 2,      // The active node the algorithm is considering. Drawn in red.
        MARKED = 4,       // Some nodes are relevant to the current operation and will be marked by the algorithm. Drawn in green.
        MARKED_E0 = 8,    // Sometimes, we need even more colours. This corresponds to whether the edge to child0 is marked.
        MARKED_E1 = 16,   // Same, but for child1.
        COSMETIC = CURRENT | MARKED | MARKED_E0 | MARKED_E1, // Used to reset the cosmetic flags which are cleared after the snapshot is taken.
        INTERMEDIATE = 32 // These nodes are used by the layout algorithm to deal with edges spanning multiple layers. They exist just in the imagination of the layout algorithm and are not drawn anywhere.
    };

    u32 child0 = 0, child1 = 0; // child0 is the false-edge, child1 the true-edge.
    u8 level = 0;
    u8 flags = 0;
    u32 id = 0; // 0 is F, the false-node, and 1 is T, the true-node
    u32 name = 0; // Index into Bdd_store::names
};

struct Snapshot {
    u64 offset_bdd;
    u64 offset_context;
};

struct Bdd_lookup {
    u64 hash; u32 id;
};

// This stores the current state of the BDD graph, as well as a copy of the graph and context for
// each snapshot.
struct Bdd_store {
    Array_t<Bdd_lookup> bdd_lookup; // Hashtable for deduplication
    s64 bdd_lookup_count; // How full the hashtable is
    Array_dyn<Bdd> bdd_data; // Maps id -> Bdd. The ids are assigned only once, so this can contain a lot of temporary nodes.

    // Data describing the current state of the algorithm. This is used to _generate_ the snapshot.
    Array_dyn<u32> snapshot_parents; // What nodes to start the reachability traversal with
    Array_dyn<u32> snapshot_cur_node; // A stack of nodes, the last of which is flagged as current
    Array_dyn<u32> snapshot_marked; // A list of nodes flagged as marked. Resets when taking a snapshot!
    Array_dyn<u32> snapshot_marked_e; // A list of edges (!) flagged as marked. Resets when taking a snapshot!
    Array_dyn<u8>  snapshot_context; // A stack of textual descriptions of the current state of the algorithm. Separated by 0.

    // This stores the snapshots.
    Array_dyn<Snapshot> snapshots; // This just contains offsets into the other two arrays. There is always a dummy element at the end.
    Array_dyn<Bdd> snapshot_data_bdd; // Nodes contained in the snapshot
    Array_dyn<u8> snapshot_data_context; // Context strings in the snapshot

    // Date for generating the names of the Bdds
    Array_dyn<s64> names; // Offsets into name_data, with a dummy element at the end. First name has length 0
    Array_dyn<u8> name_data; // utf-8 encoded names
    s64 name_next; // The final names for nodes are given in sequential order
    s64 name_next_temp; // Final names for temporary nodes
};

// Initialises the store. Can be used for re-initialisation.
void bdd_store_init(Bdd_store* store) {
    // The size of the hashmap is fixed at the constant below.
    constexpr s64 bdd_lookup_size = 4096;

    // Either allocate memory or just initialise it.
    if (store->bdd_lookup.size == 0) {
        store->bdd_lookup = array_create<Bdd_lookup>(bdd_lookup_size);
    } else {
        memset(store->bdd_lookup.data, 0, store->bdd_lookup.size * sizeof(Bdd_lookup));
    }
    assert(store->bdd_lookup.size == bdd_lookup_size);
    store->bdd_lookup_count = 0;

    store->bdd_data.size = 0;
    store->snapshot_parents.size = 0;
    store->snapshot_cur_node.size = 0;
    store->snapshot_marked.size = 0;
    store->snapshot_marked_e.size = 0;
    store->snapshot_context.size = 0;
    store->snapshots.size = 0;
    store->snapshot_data_bdd.size = 0;
    store->snapshot_data_context.size = 0;
    store->names.size = 0;
    store->name_data.size = 0;
    store->name_next = 0;
    store->name_next_temp = 0;

    // The F and T nodes.
    array_push_back(&store->bdd_data, {0, 0, 0, 0, 0, 1});
    array_push_back(&store->bdd_data, {1, 1, 0, 0, 1, 2});

    // Dummy element
    array_push_back(&store->snapshots, {0, 0});

    // Initial names for T and F as well as dummy element
    array_append(&store->names, {0ll, 0ll, 1ll, 2ll});
    array_append(&store->name_data, {(u8*)"FT", 2});
}

u64 bdd_hash(Bdd bdd) {
    // splitmix64, see http://xorshift.di.unimi.it/splitmix64.c
    // This is unnecessarily expensive, but not a bottleneck.
    u64 val = bdd.child0;
    val = (val ^ (val >> 30)) * 0xbf58476d1ce4e5b9ull;
    val = (val ^ (val >> 27)) * 0x94d049bb133111ebull;
    val = val ^ (val >> 31);
    val ^= bdd.child1;
    val = (val ^ (val >> 30)) * 0xbf58476d1ce4e5b9ull;
    val = (val ^ (val >> 27)) * 0x94d049bb133111ebull;
    val = val ^ (val >> 31);
    val ^= bdd.level | bdd.flags << 8;
    val = (val ^ (val >> 30)) * 0xbf58476d1ce4e5b9ull;
    val = (val ^ (val >> 27)) * 0x94d049bb133111ebull;
    val = val ^ (val >> 31);
    return val;
}

// Insert the bdd into the store, doing deduplication. Returns the id of the deduplicated bdd.
u32 bdd_insert(Bdd_store* store, Bdd bdd) {
    u64 hash = bdd_hash(bdd);

    if (store->bdd_lookup_count*4 >= store->bdd_lookup.size*3) {
        ui_error_report("Error: size of hash table exceeded\n"); //@Cleanup This should not use this function
        abort();
    }
    
    u64 slot = hash % store->bdd_lookup.size;
    while (store->bdd_lookup[slot].hash != 0 and store->bdd_lookup[slot].hash != hash) {
        slot = (slot + 1) % store->bdd_lookup.size;
    }
    if (store->bdd_lookup[slot].hash == 0) {
        store->bdd_lookup[slot].hash = hash;
        store->bdd_lookup[slot].id   = bdd.id;
        ++store->bdd_lookup_count;
    }
    return store->bdd_lookup[slot].id;
}

// Append a number to the next BDD name. The next time a name is assigned, it will consume this
// data.
void bdd_name_amend_number_base(Bdd_store* store, u64 number, s64 base = 10, bool small = false) {
    s64 digit_count = 0;
    for (u64 ii = number; ii or digit_count == 0; ii /= base) ++digit_count;

    array_resize(&store->name_data, store->name_data.size + digit_count);
    auto buf = array_subarray(store->name_data, store->name_data.size - digit_count, store->name_data.size);

    s64 j = buf.size;
    for (s64 j_it = 0; j_it < digit_count; ++j_it) {
        s64 digit = number % base;
        number /= base;
        buf[--j] = (small << 7) | (u8)(digit < 10 ? '0' + digit : 'a' + digit - 10);
    }
}

// Append a list of numbers to the next BDD name. The next time a name is assigned, it will consume
// this data.
void bdd_name_amend_list_base(Bdd_store* store, Array_t<u64> list, s64 base) {
    bool first = true;
    
    for (u64 i: list) {
        if (not first) array_push_back(&store->name_data, (u8)(128 | ','));
        first = false;
        bdd_name_amend_number_base(store, i, base, true);
    }
}

// Append an id based on the number to the next BDD name. These ids are of the form 'a', 'b', ...,
// 'z', 'aa', 'ab', ... and so on. The next time a name is assigned, it will consume this data.
void bdd_name_amend_id(Bdd_store* store, u32 number) {
    s64 base = 26;
    s64 digit_count = 1;
    for (u64 ii = number; ii >= base; ii = ii/base-1) ++digit_count;

    array_resize(&store->name_data, store->name_data.size + digit_count);
    auto buf = array_subarray(store->name_data, store->name_data.size - digit_count, store->name_data.size);

    s64 j = buf.size;
    for (s64 j_it = 0; j_it < digit_count; ++j_it) {
        buf[--j] = (u8)('a' + number % base);
        number = number / base - 1;
    }
}

// Append a temporary id based on the number to the next BDD name. These temporary ids are of the
// form '%0', '%1', ... and so on. The next time a name is assigned, it will consume this data.
void bdd_name_amend_tempid(Bdd_store* store, u32 number) {
    array_printf(&store->name_data, "%");
    bdd_name_amend_number_base(store, number);
}

// Append an existing bdd name to the next BDD name. The next time a name is assigned, it will
// consume this data.
void bdd_name_amend_name(Bdd_store* store, u32 bdd) {
    u32 name = store->bdd_data[bdd].name;
    auto arr = array_subarray(store->name_data, store->names[name], store->names[name+1]);
    array_append(&store->name_data, arr);
}

// Append a name representing a set operation to the bdd name. The next time a name is assigned, it
// will consume this data.
void bdd_name_amend_setop(Bdd_store* store, u32 a, u32 b, char op) {
    assert(store->names.size > 0);
    bdd_name_amend_name(store, a);
    array_push_back(&store->name_data, (u8)op);
    bdd_name_amend_name(store, b);
}

// Assign the next bdd name. This will consume the data given by the bdd_name_amend function.
void bdd_name_assign(Bdd_store* store, Bdd* bdd) {
    assert(store->names.size > 0);
    if (store->name_data.size != store->names[store->names.size-1]) {
        bdd->name = store->names.size-1;
        store->bdd_data[bdd->id].name = bdd->name;
        array_push_back(&store->names, store->name_data.size);
    }
}

// Create a new bdd, with a new id. Unless the node is temporary, this does both deduplication and
// removal of unnecessary nodes (both children point to the same node).
//  Additionally, if there is any data remaining in store->name_data, assign it as the new name of
// the node.
void bdd_create_inplace(Bdd_store* store, Bdd* bdd) {
    bdd->id = store->bdd_data.size;
    if (bdd->flags & Bdd::TEMPORARY) {
        array_push_back(&store->bdd_data, *bdd);
    } else {
        if (bdd->child0 == bdd->child1) {
            *bdd = store->bdd_data[bdd->child0];
        } else {
            bdd->id = bdd_insert(store, *bdd);
            if (bdd->id == store->bdd_data.size) {
                array_push_back(&store->bdd_data, *bdd);
                bdd_name_amend_id(store, store->name_next++);
                bdd_name_assign(store, bdd);
            } else {
                *bdd = store->bdd_data[bdd->id];
            }
        }
    }

    bdd_name_assign(store, bdd);
}
u32 bdd_create(Bdd_store* store, Bdd bdd) {
    bdd_create_inplace(store, &bdd);
    return bdd.id;
}

// Partition numbers by using the bit with index level. (The least-significant bit has index 0.)
// Returns the index of the first element of the second set, so that the partition is given by [0,
// index), [index, numbers->size).
s64 _partition_along_bit(Array_t<u64>* numbers, u8 level) {
    u64 l = 0;
    for (u64 r = 0; r < (u64)numbers->size; ++r) {
        if (not ((*numbers)[r] >> level & 1)) {
            u64 tmp = (*numbers)[l];
            (*numbers)[l] = (*numbers)[r];
            (*numbers)[r] = tmp;
            ++l;
        }
    }

    return l;
}

// Create a new bdd representing numbers. This does not actually take any snapshots or do any
// visualisation work, it is just a straightforward implementation of the algorithm. (This function
// is used as a subroutine by the actual stepwise algorithm.)
u32 bdd_from_list(Bdd_store* store, Array_t<u64> numbers, Array_t<u8> levels) {
    assert(store);
    assert(levels.size < 256);
    
    if (numbers.size == 0) {
        return 0;
    } else if (levels.size == 0) {
        return 1;
    }
    
    u8 level = levels[0];

    s64 index = _partition_along_bit(&numbers, level);

    auto levels_sub = array_subarray(levels, 1, levels.size);
    u32 child0 = bdd_from_list(store, array_subarray(numbers, 0, index), levels_sub);
    u32 child1 = bdd_from_list(store, array_subarray(numbers, index, numbers.size), levels_sub);

    return bdd_create(store, {child0, child1, (u8)levels.size});
}

// This pushed a new messages onto the context stack. printf-like function.
template <typename... Args>
void context_append_(Bdd_store* store, char const* msg, Args... args) {
    u64 len = snprintf(nullptr, 0, msg, args...);
    array_reserve(&store->snapshot_context, store->snapshot_context.size + len+1);
    snprintf(
        (char*)store->snapshot_context.end(),
        store->snapshot_context.capacity - store->snapshot_context.size,
        msg, args...
    );
    store->snapshot_context.size += len+1;
}
void context_append_(Bdd_store* store, char const* msg) {
    array_append(&store->snapshot_context, {(u8*)msg, (s64)(strlen(msg) + 1)});
}
// Append the string to the last message currently on the context stack.
template <typename... Args>
void context_amend_(Bdd_store* store, char const* msg, Args... args) {
    --store->snapshot_context.size;
    context_append_(store, msg, args...);
}

// Use this to get the compiler to check that the printf arguments are passed correctly. (I tried to get the built-in annotations to work, but did not.) Debug functionality.
#if 0
#define context_append(a, ...) context_append_(a,  __VA_ARGS__); snprintf(0, 0, __VA_ARGS__)
#define context_amend(a, ...) context_amend_(a,  __VA_ARGS__); snprintf(0, 0, __VA_ARGS__)
#else
#define context_append context_append_
#define context_amend context_amend_
#endif

void context_amend(Bdd_store* store, Array_t<u8> msg) {
    --store->snapshot_context.size;
    array_append(&store->snapshot_context, msg);
    array_push_back(&store->snapshot_context, (u8)0);
}

// Like context_amend, but generate a list of numbers in square braces, separated by commas. fmt
// specifies the format to use for each number. If you notice anything strange here, it may pay of
// to use the above macro to make sure you got the printf specifiers right.
template <typename T>
void context_amend_list(Bdd_store* store, Array_t<T> list, char const* fmt) {
    context_amend(store, "[");
    bool first = true;
    for (T i: list) {
        if (not first) context_amend(store, ", ");
        first = false;
        context_amend(store, fmt, i);
    }
    context_amend(store, "]");
}

// Like context_amend, but write number in base base with digit_count digits. If digit_count is -1,
// then it will be determined automatically. marked contains digits that should be shown in bold.
void context_amend_number_base(Bdd_store* store, u64 number, s64 base, s64 digit_count = -1, Array_t<u8> marked = {}) {
    // If there is no fixed count, determine the actual amount here
    if (digit_count == -1) {
        digit_count = 0;
        for (u64 ii = number; ii or digit_count == 0; ii /= base) ++digit_count;

        for (u8 pos: marked) {
            digit_count = std::max(digit_count, (s64)pos+1);
        }
    }

    s64 buf_size = digit_count + 7 * marked.size + 1;
    Array_t<char> buf = {(char*)alloca(buf_size), buf_size};

    s64 j = buf.size;
    buf[--j] = 0;
    bool marking = false;
    for (s64 j_it = 0; j_it < digit_count; ++j_it) {
        s64 digit = number % base;
        number /= base;
            
        char c = (char)(digit < 10 ? '0' + digit : 'a' + digit - 10);

        bool flag = false;
        for (u8 pos: marked) flag |= pos == j_it;
        if (not flag and marking) {
            j -= 3;
            memcpy(&buf[j], "<b>", 3);
        }
        if (flag and not marking) {
            j -= 4;
            memcpy(&buf[j], "</b>", 4);
        }
        marking = flag;
        
        buf[--j] = c;
    }
    
    if (marking) {
        j -= 3;
        memcpy(&buf[j], "<b>", 3);
    }
    
    context_amend(store, &buf[j]);
}
void context_amend_number_base(Bdd_store* store, u64 number, s64 base, s64 digit_count, s64 digit_mark_) {
    if (digit_mark_ == -1) {
        context_amend_number_base(store, number, base, digit_count);
    } else {
        u8 digit_mark = (u8)digit_mark_;
        context_amend_number_base(store, number, base, digit_count, {&digit_mark, 1});
    }
}

// Like context_amend_list, but prints the numbers ith context_amend_number_base instead of printf.
void context_amend_list_base(Bdd_store* store, Array_t<u64> list, s64 base, s64 digit_count = -1, s64 digit_mark = -1) {
    context_amend(store, "[");
    bool first = true;
    
    for (u64 i: list) {
        if (not first) context_amend(store, ", ");
        first = false;
        context_amend_number_base(store, i, base, digit_count, digit_mark);
    }
    context_amend(store, "]");
}

// Pop the last element of the context stack. This will assert if it is empty.
void context_pop(Bdd_store* store) {
    --store->snapshot_context.size;
    while (store->snapshot_context.size and store->snapshot_context[store->snapshot_context.size-1])
        --store->snapshot_context.size;
}

// Note on bdd labels: The names are stored with one byte per character. Why not use the existing
// system for formatted text you ask? Two reasons: The labels need to be rendered by GL, even in the
// browser. Hence I cannot use the same backend I use for the other formatted text, which emit
// HTML. Additionally, the names are stored in the Bdd_store, and the formatted text API would need
// additional interfaces to enable outside storage. So it is not really a good fit here.
//  Instead I store the labels as simple sequences of bytes with an ASCII-like encoding. The range
// [32,127] is mapped to itself and contains the printable characters. While drawing, a few of these
// will be mapped to other unicode glyphs, e.g. & becomes set union. [22,32) are then mapped to
// subscripts. Finally, some of the labels are written in a smaller font, so that they can be
// wrapped into two lines. The small characters are mapped in exactly the same fashion, but with
// their high-bit set.
//  Finally, the bytes are then mapped to indices into the texture in a simple fashio, we map
// [22,127) to [0,105), and then 128 | [22,127) to 105 + [0,105).

constexpr s64 opengl_bddlabel_index_size = 210; // Total number of slots in the texture

// Return the index of bytes c according to the above mapping and the other way around.
s64 opengl_bddlabel_char_index(u8 c) {
    if (22 <= (c&127) and (c&127) < 127) {
        return (c&127) - 22 + (c>>7) * 105;
    } else {
        return -1;
    }
}
u8 opengl_bddlabel_index_char(s64 index) {
    assert(0 <= index and index < 210);
    return (u8)(index < 105 ? index + 22 : index + 45);
}

// Convert an index into the texture into a unicode codepoint represented in UTF-8 as well as
// information on how to draw it.
Array_t<u8> opengl_bddlabel_index_utf8(s64 index, bool* draw_light_ = nullptr, bool* draw_italics_=nullptr) {
    assert(0 <= index and index < 210);
    static char c[2];
    c[0] = (char)(index < 105 ? index + 22 : index - 83);
    char const* s;
    bool draw_light = false;
    bool draw_italics = 'a' <= c[0] and c[0] <= 'z';
    switch (c[0]) {
    case '|': s = u8"∪"; draw_light = true; break;
    case '&': s = u8"∩"; draw_light = true; break;
    case '~': s = u8"¬"; draw_light = true; break;
    case '+': s = u8"∨"; draw_light = true; break;
    case '-': s = u8"∧"; draw_light = true; break;
    case '<': s = u8"←"; draw_light = true; break;
    case '>': s = u8"→"; draw_light = true; break;
    case '=': s = u8"↔"; draw_light = true; break;
    case '^': s = u8"⊕"; draw_light = true; break;
    case '?': s = u8"∀"; break;
    case '!': s = u8"∃"; break;
    case 22+0: s = u8"₀"; break;
    case 22+1: s = u8"₁"; break;
    case 22+2: s = u8"₂"; break;
    case 22+3: s = u8"₃"; break;
    case 22+4: s = u8"₄"; break;
    case 22+5: s = u8"₅"; break;
    case 22+6: s = u8"₆"; break;
    case 22+7: s = u8"₇"; break;
    case 22+8: s = u8"₈"; break;
    case 22+9: s = u8"₉"; break;
    default: s = &c[0]; break;
    };
    if (draw_light_)   *draw_light_   = draw_light;
    if (draw_italics_) *draw_italics_ = draw_italics;
    return {(u8*)s, (s64)strlen(s)};
}

Array_t<u8> opengl_bddlabel_char_utf8(u8 c) {
    return opengl_bddlabel_index_utf8(opengl_bddlabel_char_index(c));
}
    
// Like context_amend, but formats the name of a bdd.
void context_amend_bdd(Bdd_store* store, u32 id) {
    u32 name = store->bdd_data[id].name;
    auto arr = array_subarray(store->name_data, store->names[name], store->names[name+1]);

    if (id <= 1) context_amend(store, "<s>");
    bool italics_on = false;
    for (u8 c: arr) {
        bool italicized;
        auto c_arr = opengl_bddlabel_index_utf8(opengl_bddlabel_char_index(c), nullptr, &italicized);
        if (not italics_on and italicized) context_amend(store, "<i>");
        if (italics_on and not italicized) context_amend(store, "</i>");
        italics_on = italicized;
        context_amend(store, c_arr);
    }
    if (italics_on) context_amend(store, "</i>");
    if (id <= 1) context_amend(store, "</s>");
}

// Take a temporary bdd and finalise it, i.e. do deduplication and removal of unnecessary
// nodes. This will push a description of the operation performed onto the context stack. (If
// nothing was done, an empty entry is pushed.)
//  If the bdd had a name, it will be preserved.
u32 bdd_finalize(Bdd_store* store, Bdd bdd) {
    // @Cleanup: Give temporary nodes names
    assert(bdd.flags & Bdd::TEMPORARY);
    bdd.flags &= ~Bdd::TEMPORARY;
    
    if (bdd.child0 == bdd.child1) {
        bdd_name_amend_tempid(store, store->name_next_temp++);
        bdd_name_assign(store, &bdd);
            
        context_append(store, "Node ");
        context_amend_bdd(store, bdd.id);
        context_amend(store, " is superflous, remove");
        return bdd.child0;
    } else {
        u32 bdd_id = bdd_insert(store, bdd);
        if (bdd_id == bdd.id) {
            store->bdd_data[bdd.id] = bdd;
            bdd_name_amend_id(store, store->name_next++);
            bdd_name_assign(store, &bdd);
            context_append(store, "");
        } else {
            bdd_name_amend_tempid(store, store->name_next_temp++);
            bdd_name_assign(store, &bdd);
            
            context_append(store, "Merging node ");
            context_amend_bdd(store, bdd.id);
            context_amend(store, " with ");
            context_amend_bdd(store, bdd_id);
        }
        return bdd_id;
    }
}

// Store the current state of the store into a snapshot, together with the additional visualisation
// information.
void take_snapshot(Bdd_store* store) {
    // Compute the reachable set of nodes from the ones in snapshot_parents
    Array_dyn<u32> reachable;
    Array_t<u64> reached {(u64*)calloc((store->bdd_data.size+63)/64, sizeof(u64)), (store->bdd_data.size+63)/64};
    
    defer { array_free(&reachable); };
    defer { array_free(&reached); };

    for (u32 i: store->snapshot_parents) {
        if (not bitset_get(reached, i)) {
            array_push_back(&reachable, i);
            bitset_set(&reached, i, 1);
        }
    }
    bitset_set(&reached, 0, 1);

    for (s64 i = 0; i < reachable.size; ++i) {
        Bdd bdd = store->bdd_data[reachable[i]];
        // Works implicitly for T and F
        if (not bitset_get(reached, bdd.child0)) {
            array_push_back(&reachable, bdd.child0);
            bitset_set(&reached, bdd.child0, 1);
        }
        if (not bitset_get(reached, bdd.child1)) {
            array_push_back(&reachable, bdd.child1);
            bitset_set(&reached, bdd.child1, 1);
        }
    }

    // Set the cosmetic flags
    for (Bdd& i: store->bdd_data) {
        i.flags &= ~Bdd::COSMETIC;
    }
    if (store->snapshot_cur_node.size) {
        u32 cur = store->snapshot_cur_node[store->snapshot_cur_node.size-1];
        store->bdd_data[cur].flags |= Bdd::CURRENT;
    }
    for (u32 i: store->snapshot_marked) {
        store->bdd_data[i].flags |= Bdd::MARKED;
    }
    for (u32 i: store->snapshot_marked_e) {
        store->bdd_data[i >> 1].flags |= i & 1 ? Bdd::MARKED_E1 : Bdd::MARKED_E0;
    }

    // Save nodes and context into the snapshot
    for (u32 i: reachable) {
        array_push_back(&store->snapshot_data_bdd, store->bdd_data[i]);
    }
    array_append(&store->snapshot_data_context, store->snapshot_context);
    // Commit the snapshot
    array_push_back(&store->snapshots, {(u64)store->snapshot_data_bdd.size, (u64)store->snapshot_data_context.size});

    // Reset the marked nodes
    store->snapshot_marked.size = 0;
    store->snapshot_marked_e.size = 0;
}


// Note on stepwise functions: bdd_union_stepwise, bdd_intersection_stepwise,
// bdd_complement_stepwise, and bdd_from_list_stepwise do the heavy lifting in executing the
// algorithms in a step-by-step manner. There are some key points regarding their implementation:
//  - They take an additional bdd argument, which is the id of the bdd they should write their
//    result into. This is only used for recursive calls.
//  - Recursive calls will leave one item on the context stack from the bdd_finalize call.
//  - Children are created by the parent call and then populated by recusive calls, using the bdd
//    argument.
//  - Management of the context stack is done very carefully, so that nothing remains at the end.
// Out of these functions, only bdd_union stepwise has general comments regarding the common
// functionality.

// Compute the union of a and b while doing all the visualisation work. The bdd argument is meant
// for recursive calls, no need to use it.
u32 bdd_union_stepwise(Bdd_store* store, u32 a, u32 b, u32 bdd = -1, u32 a_parent = -1, u32 b_parent = -1) {
    // Duplicated code below in bdd_intersection_stepwise. Take care. Also see the note there.
    //@Cleanup: Update the others
    
    Bdd a_bdd = store->bdd_data[a];
    Bdd b_bdd = store->bdd_data[b];

    // Check whether we are the top-level call or in a recursive call
    Bdd bdd_temp;
    if (bdd == (u32)-1) {
        // Create a new node for the union
        bdd_temp = {0, 0, std::max(a_bdd.level, b_bdd.level), Bdd::TEMPORARY};
        bdd_name_amend_setop(store, a, b, '|');
        bdd_create_inplace(store, &bdd_temp);
        array_push_back(&store->snapshot_parents, bdd_temp.id); // Mark it as parent, so that snapshots capture it and its descenndants.
        
        context_append(store, "Calculating union of ");
    } else {
        // In a recursive call, we get passed a node into which we construct the result
        bdd_temp = store->bdd_data[bdd];
        
        context_append(store, "Doing union of ");
    }
    
    context_amend_bdd(store, a);
    if (a == 0 and a_parent != -1) {
        context_amend(store, " (node ");
        context_amend_bdd(store, a_parent >> 1);
        context_amend(store, a_parent & 1 ? " has no child 1)" : " has no child 0)");
    }
    context_amend(store, " and ");
    context_amend_bdd(store, b);
    if (b == 0 and b_parent != -1) {
        context_amend(store, " (node ");
        context_amend_bdd(store, b_parent >> 1);
        context_amend(store, b_parent & 1 ? " has no child 1)" : " has no child 0)");
    }

    // Set the current node and take snapshot
    array_push_back(&store->snapshot_cur_node, bdd_temp.id);
    array_append(&store->snapshot_marked, {a, b});
    if (a_parent != -1) array_push_back(&store->snapshot_marked_e, a_parent);
    if (b_parent != -1) array_push_back(&store->snapshot_marked_e, b_parent);
    take_snapshot(store);

    // Now there are a bunch of special cases. Only a few of them are actually necessary to
    // implement the algorithm, but the visualisation is much more tedious if they are not
    // considered.
    
    u32 bdd_final;
    if (a == 1 or b == 1) {
        // One node is T
        bdd_final = 1;
        context_pop(store);
        context_append(store, a == 1 ? "First" : "Second");
        context_amend(store, " node is <s>T</s>, connect to <s>T</s>");
    } else if (a == 0 or b == 0) {
        // One node is F
        bdd_final = a == 0 ? b : a;
        context_pop(store);
        if (bdd_final == 0) {
            context_append(store, "Both nodes are <s>F</s>, no connection");
        } else {
            context_append(store, a == 0 ? "First" : "Second");
            context_amend(store, " node is <s>F</s>, connect to ");
            context_amend_bdd(store, bdd_final);
        }
    } else if (a == b) {
        // Both nodes are equal
        context_pop(store);
        context_append(store, "The nodes are equal, connect");
        bdd_final = a;
    } else if (std::max(a_bdd.level, b_bdd.level) == 1) {
        // We are at the last level. This one is not necessary.
        
        context_append(store, "Both nodes (");
        context_amend_bdd(store, a);
        context_amend(store, " and ");
        context_amend_bdd(store, b);
        context_amend(store, ") at last level, direct connections");
        array_append(&store->snapshot_marked, {a, b});
        take_snapshot(store);
        context_pop(store);
        
        // We know that we are at the last level, so the only possible children are F and T, having
        // ids 0 and 1, respectively. Hence, we can use bit-operations on the ids.
        bdd_temp.child0 = a_bdd.child0 | b_bdd.child0;
        store->bdd_data[bdd_temp.id] = bdd_temp; // Write back changes into the store

        context_append(store, "Child 0 is ");
        context_amend_bdd(store, bdd_temp.child0);
        context_amend(store, ", union of ");
        context_amend_bdd(store, a_bdd.child0);
        context_amend(store, " and ");
        context_amend_bdd(store, b_bdd.child0);
        array_append(&store->snapshot_marked, {a_bdd.child0, b_bdd.child0});
        take_snapshot(store);
        context_pop(store);

        // See above.
        bdd_temp.child1 = a_bdd.child1 | b_bdd.child1;
        store->bdd_data[bdd_temp.id] = bdd_temp; // Write back changes into the store
        
        context_append(store, "Child 1 is ");
        context_amend_bdd(store, bdd_temp.child1);
        context_amend(store, ", union of ");
        context_amend_bdd(store, a_bdd.child1);
        context_amend(store, " and ");
        context_amend_bdd(store, b_bdd.child1);
        array_append(&store->snapshot_marked, {a_bdd.child1, b_bdd.child1});
        take_snapshot(store);
        context_pop(store);

        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    } else {
        // The general case
        
        u8 child_level = std::max(a_bdd.level, b_bdd.level) - 1;
        if (a_bdd.level > b_bdd.level) {
            context_append(store, "First node (");
            context_amend_bdd(store, a);
            context_amend(store, ") has higher level, take its branches");
            array_push_back(&store->snapshot_marked, a);
        } else if (a_bdd.level < b_bdd.level) {
            context_append(store, "Second node (");
            context_amend_bdd(store, b);
            context_amend(store, ") has higher level, take its branches");
            array_push_back(&store->snapshot_marked, a);
        } else {
            context_append(store, "Both nodes (");
            context_amend_bdd(store, a);
            context_amend(store, " and ");
            context_amend_bdd(store, b);
            context_amend(store, ") at same level, take children of both");
            array_append(&store->snapshot_marked, {a, b});
        }

        // child00 and child01 will be combined to form child0, same for child1
        u32 child00_par = a, child01_par = b, child00_parent = 0, child01_parent = 0;
        u32 child10_par = a, child11_par = b, child10_parent = 0, child11_parent = 0;
        if (a_bdd.level >= b_bdd.level) {
            // Take a's children
            child00_par = a_bdd.child0;
            child10_par = a_bdd.child1;
            child00_parent = a << 1 | 0;
            child10_parent = a << 1 | 1;
        }
        if (b_bdd.level >= a_bdd.level) {
            // Take b's children
            child01_par = b_bdd.child0;
            child11_par = b_bdd.child1;
            child01_parent = b << 1 | 0;
            child11_parent = b << 1 | 1;
        }        

        // Both children are created as temporary nodes, the recursive calls will populate them
        bdd_name_amend_setop(store, child00_par, child01_par, '|');
        bdd_temp.child0 = bdd_create(store, {0, 0, child_level, Bdd::TEMPORARY});
        bdd_name_amend_setop(store, child10_par, child11_par, '|');
        bdd_temp.child1 = bdd_create(store, {0, 0, child_level, Bdd::TEMPORARY});
        store->bdd_data[bdd_temp.id] = bdd_temp; // Write back changes into the store

        take_snapshot(store);

        // Recusively fill in the children
        
        bdd_temp.child0 = bdd_union_stepwise(store, child00_par, child01_par, bdd_temp.child0, child00_parent, child01_parent);
        store->bdd_data[bdd_temp.id] = bdd_temp; // Write back changes into the store
        take_snapshot(store);
        context_pop(store);
        --store->snapshot_cur_node.size;
        
        bdd_temp.child1 = bdd_union_stepwise(store, child10_par, child11_par, bdd_temp.child1, child10_parent, child11_parent);
        store->bdd_data[bdd_temp.id] = bdd_temp; // Write back changes into the store
        take_snapshot(store);
        context_pop(store);
        --store->snapshot_cur_node.size;

        context_pop(store);
        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    }

    // Our id may have changed, so update it.
    store->snapshot_cur_node[store->snapshot_cur_node.size-1] = bdd_final;
    if (bdd == (u32)-1) {
        // This is the end of the algorithm.
        store->snapshot_parents[store->snapshot_parents.size-1] = bdd_final;
        --store->snapshot_cur_node.size;
        context_append(store, "Done.");
        take_snapshot(store);
        context_pop(store);
        context_pop(store);
    }
    return bdd_final;
}

// See note on stepwise functions above.
u32 bdd_intersection_stepwise(Bdd_store* store, u32 a, u32 b, u32 bdd = -1, u32 a_parent = -1, u32 b_parent = -1) {
    // This is the same code as in bdd_union_stepwise. Could be somewhat easily merged into a single
    // function, though readability will suffer.
    
    Bdd a_bdd = store->bdd_data[a];
    Bdd b_bdd = store->bdd_data[b];

    Bdd bdd_temp;
    if (bdd == (u32)-1) {
        bdd_temp = {0, 0, std::max(a_bdd.level, b_bdd.level), Bdd::TEMPORARY};
        bdd_temp.id = bdd_create(store, bdd_temp);
        array_push_back(&store->snapshot_parents, bdd_temp.id);

        context_append(store, "Calculating intersection of ");
    } else {
        bdd_temp = store->bdd_data[bdd];
        
        context_append(store, "Doing intersection of ");
    }
    
    context_amend_bdd(store, a);
    if (a == 0 and a_parent != -1) {
        context_amend(store, " (node ");
        context_amend_bdd(store, a_parent >> 1);
        context_amend(store, a_parent & 1 ? " has no child 1)" : " has no child 0)");
    }
    context_amend(store, " and ");
    context_amend_bdd(store, b);
    if (b == 0 and b_parent != -1) {
        context_amend(store, " (node ");
        context_amend_bdd(store, b_parent >> 1);
        context_amend(store, b_parent & 1 ? " has no child 1)" : " has no child 0)");
    }
    context_amend(store, " for node ");
    context_amend_bdd(store, bdd_temp.id);

    array_push_back(&store->snapshot_cur_node, bdd_temp.id);
    array_append(&store->snapshot_marked, {a, b});
    if (a_parent != -1) array_push_back(&store->snapshot_marked_e, a_parent);
    if (b_parent != -1) array_push_back(&store->snapshot_marked_e, b_parent);
    take_snapshot(store);

    u32 bdd_final;
    if (a == 0 or b == 0) {
        bdd_final = 0;
        context_pop(store);
        context_append(store, a == 0 ? "First" : "Second");
        context_amend(store, " node is <s>F</s>, no connection");
    } else if (a == 1 or b == 1) {
        bdd_final = a == 1 ? b : a;
        context_pop(store);
        if (bdd_final == 1) {
            context_append(store, "Both nodes are <s>T</s>, connect to <s>T</s>");
        } else {
            context_append(store, a == 1 ? "First" : "Second");
            context_amend(store, " node is <s>T</s>, connect to ");
            context_amend_bdd(store, bdd_final);
        }
    } else if (a == b) {
        context_pop(store);
        context_append(store, "The nodes are equal, connect");
        bdd_final = a;
    } else if (std::max(a_bdd.level, b_bdd.level) == 1) {
        context_append(store, "Both nodes (");
        context_amend_bdd(store, a);
        context_amend(store, " and ");
        context_amend_bdd(store, b);
        context_amend(store, ") at last level, direct connections");
        array_append(&store->snapshot_marked, {a, b});
        take_snapshot(store);
        context_pop(store);

        bdd_temp.child0 = a_bdd.child0 & b_bdd.child0;
        store->bdd_data[bdd_temp.id] = bdd_temp;

        context_append(store, "Child 0 is ");
        context_amend_bdd(store, bdd_temp.child0);
        context_amend(store, ", intersection of ");
        context_amend_bdd(store, a_bdd.child0);
        context_amend(store, " and ");
        context_amend_bdd(store, b_bdd.child0);
        array_append(&store->snapshot_marked, {a_bdd.child0, b_bdd.child0});
        take_snapshot(store);
        context_pop(store);
        
        bdd_temp.child1 = a_bdd.child1 & b_bdd.child1;
        store->bdd_data[bdd_temp.id] = bdd_temp;
        
        context_append(store, "Child 1 is ");
        context_amend_bdd(store, bdd_temp.child1);
        context_amend(store, ", intersection of ");
        context_amend_bdd(store, a_bdd.child1);
        context_amend(store, " and ");
        context_amend_bdd(store, b_bdd.child1);
        array_append(&store->snapshot_marked, {a_bdd.child1, b_bdd.child1});
        take_snapshot(store);
        context_pop(store);

        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    } else {
        u8 child_level = std::max(a_bdd.level, b_bdd.level) - 1;
        if (a_bdd.level > b_bdd.level) {
            context_append(store, "First node (");
            context_amend_bdd(store, a);
            context_amend(store, ") has higher level, take its branches");
            array_push_back(&store->snapshot_marked, a);
        } else if (a_bdd.level < b_bdd.level) {
            context_append(store, "Second node (");
            context_amend_bdd(store, b);
            context_amend(store, ") has higher level, take its branches");
            array_push_back(&store->snapshot_marked, a);
        } else {
            context_append(store, "Both nodes (");
            context_amend_bdd(store, a);
            context_amend(store, " and ");
            context_amend_bdd(store, b);
            context_amend(store, ") at same level, take children of both");
            array_append(&store->snapshot_marked, {a, b});
        }

        u32 child00_par = a, child01_par = b, child00_parent = 0, child01_parent = 0;
        u32 child10_par = a, child11_par = b, child10_parent = 0, child11_parent = 0;
        if (a_bdd.level >= b_bdd.level) {
            child00_par = a_bdd.child0;
            child10_par = a_bdd.child1;
            child00_parent = a << 1 | 0;
            child10_parent = a << 1 | 1;
        }
        if (b_bdd.level >= a_bdd.level) {
            child01_par = b_bdd.child0;
            child11_par = b_bdd.child1;
            child01_parent = b << 1 | 0;
            child11_parent = b << 1 | 1;
        }        

        bdd_temp.child0 = bdd_create(store, {0, 0, child_level, Bdd::TEMPORARY});
        bdd_temp.child1 = bdd_create(store, {0, 0, child_level, Bdd::TEMPORARY});
        store->bdd_data[bdd_temp.id] = bdd_temp;

        take_snapshot(store);

        bdd_temp.child0 = bdd_intersection_stepwise(store, child00_par, child01_par, bdd_temp.child0, child00_parent, child01_parent);
        store->bdd_data[bdd_temp.id] = bdd_temp;
        take_snapshot(store);
        context_pop(store);
        --store->snapshot_cur_node.size;

        bdd_temp.child1 = bdd_intersection_stepwise(store, child10_par, child11_par, bdd_temp.child1, child10_parent, child11_parent);
        store->bdd_data[bdd_temp.id] = bdd_temp;
        take_snapshot(store);
        context_pop(store);
        --store->snapshot_cur_node.size;

        context_pop(store);
        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    }

    store->snapshot_cur_node[store->snapshot_cur_node.size-1] = bdd_final;
    if (bdd == (u32)-1) {
        store->snapshot_parents[store->snapshot_parents.size-1] = bdd_final;
        --store->snapshot_cur_node.size;
        context_append(store, "Done.");
        take_snapshot(store);
        context_pop(store);
        context_pop(store);
    }
    return bdd_final;
}

// See note on stepwise functions above.
u32 bdd_complement_stepwise(Bdd_store* store, u32 a, u32 bdd = -1) {
    Bdd a_bdd = store->bdd_data[a];    

    Bdd bdd_temp;
    if (bdd == (u32)-1) {
        bdd_temp = {0, 0, a_bdd.level, Bdd::TEMPORARY};
        bdd_temp.id = bdd_create(store, bdd_temp);
        context_append(store, "Calculating complement of ");
        context_amend_bdd(store, a);
        context_amend(store, " as node ");
        context_amend_bdd(store, bdd_temp.id);
        array_push_back(&store->snapshot_parents, bdd_temp.id);
    } else {
        bdd_temp = store->bdd_data[bdd];
        context_append(store, "Doing complement of ");
        context_amend_bdd(store, a);
        context_amend(store, " for node ");
        context_amend_bdd(store, bdd_temp.id);
    }

    array_push_back(&store->snapshot_cur_node, bdd_temp.id);
    array_push_back(&store->snapshot_marked, a);
    take_snapshot(store);

    u32 bdd_final;
    if (a < 2) {
        bdd_final = not a;
        context_pop(store);
        context_append(store, "Node is ");
        context_amend_bdd(store, a);
        context_amend(store, ", connect to ");
        context_amend_bdd(store, bdd_final);
    } else if (a_bdd.level == 1) {
        context_append(store, "Node at last level, direct connections");
        array_push_back(&store->snapshot_marked, a);
        take_snapshot(store);
        context_pop(store);

        bdd_temp.child0 = not a_bdd.child0;
        bdd_temp.child1 = not a_bdd.child1;
        store->bdd_data[bdd_temp.id] = bdd_temp;
        
        context_append(store, "Connect first child to ");
        context_amend_bdd(store, bdd_temp.child0);
        context_amend(store, ", second child to ");
        context_amend_bdd(store, bdd_temp.child1);

        array_push_back(&store->snapshot_marked, a);
        take_snapshot(store);
        context_pop(store);

        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    } else {
        bdd_temp.child0 = bdd_create(store, {0, 0, (u8)(bdd_temp.level-1), Bdd::TEMPORARY});
        bdd_temp.child1 = bdd_create(store, {0, 0, (u8)(bdd_temp.level-1), Bdd::TEMPORARY});
        store->bdd_data[bdd_temp.id] = bdd_temp;

        context_append(store, "Negate both children");
        
        take_snapshot(store);
        context_pop(store);

        bdd_temp.child0 = bdd_complement_stepwise(store, a_bdd.child0, bdd_temp.child0);
        store->bdd_data[bdd_temp.id] = bdd_temp;
        take_snapshot(store);
        context_pop(store);
        --store->snapshot_cur_node.size;

        bdd_temp.child1 = bdd_complement_stepwise(store, a_bdd.child1, bdd_temp.child1);
        store->bdd_data[bdd_temp.id] = bdd_temp;
        take_snapshot(store);
        context_pop(store);
        --store->snapshot_cur_node.size;

        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    }

    store->snapshot_cur_node[store->snapshot_cur_node.size-1] = bdd_final;
    if (bdd == (u32)-1) {
        store->snapshot_parents[store->snapshot_parents.size-1] = bdd_final;
        --store->snapshot_cur_node.size;
        context_append(store, "Done.");
        take_snapshot(store);
        context_pop(store);
        context_pop(store);
    }
    return bdd_final;
}

// See note on stepwise functions above.
u32 bdd_from_list_stepwise(Bdd_store* store, Array_t<u64> numbers, Array_t<u8> levels, s64 base, u32 bdd = -1, s64 levels_total = -1) {
    assert(levels.size < 32);
    u8 level = levels[0];

    Bdd bdd_temp;
    if (bdd == (u32)-1) {
        context_append(store, "Creating BDD from list ");
        context_amend_list_base(store, numbers, base);
        context_amend(store, " using bit order ");
        context_amend_list(store, levels, "%hhd");

        bdd_name_amend_list_base(store, numbers, base);
        
        bdd_temp = {0, 0, (u8)levels.size, Bdd::TEMPORARY};
        bdd_create_inplace(store, &bdd_temp);
        array_push_back(&store->snapshot_parents, bdd_temp.id);

        levels_total = levels.size;
    } else {
        bdd_temp = store->bdd_data[bdd];
        context_append(store, "Processing sublist ");
        context_amend_list_base(store, numbers, base);
    }

    array_push_back(&store->snapshot_cur_node, bdd_temp.id);
    take_snapshot(store);

    u32 bdd_final;
    if (numbers.size == 0) {
        // No items left
        bdd_final = 0;
        context_pop(store);
        context_append(store, "List is empty, no connection");
    } else if (levels.size == 0) {
        // No layers left, but still some items in there.
        bdd_final = 1;
        context_pop(store);
        context_append(store, "All layers done, connect to <s>T</s>");
    } else if (numbers.size == 1) {
        // Only one number remains. Unnecessary in principle.
        context_pop(store);
        context_append(store, "Only item %lld (", numbers[0]);
        context_amend_number_base(store, numbers[0], 2, -1, levels);
        context_amend(store, ") remains, connect directly");
        u32 child = bdd_from_list(store, numbers, array_subarray(levels, 1, levels.size));
        if ((numbers[0] >> levels[0] & 1) == 0) {
            bdd_temp.child0 = child;
            bdd_temp.child1 = 0;
        } else {
            bdd_temp.child0 = 0;
            bdd_temp.child1 = child;
        }
        bdd_final = bdd_finalize(store, bdd_temp);
        context_pop(store);
    } else if (levels.size == 1) {
        // Handle the last level specially, as I do not want to create temporary nodes on the last
        // level.
        s64 index = _partition_along_bit(&numbers, level);
        auto lst0 = array_subarray(numbers, 0, index);
        auto lst1 = array_subarray(numbers, index, numbers.size);
        
        context_append(store, "Splitting at bit %hhd (last level). The child 0 sublist is ", level);
        if (base != 2) {
            context_amend_list_base(store, lst0, 2, levels_total, level);
            context_amend(store, " (i.e. ");
            context_amend_list_base(store, lst0, base);
            context_amend(store, "), the child 1 sublist is ");
            context_amend_list_base(store, lst1, 2, levels_total, level);
            context_amend(store, " (i.e. ");
            context_amend_list_base(store, lst1, base);
            context_amend(store, ")");
        } else {
            context_amend_list_base(store, lst0, 2, levels_total, level);
            context_amend(store, ", the child 1 sublist is ");
            context_amend_list_base(store, lst1, 2, levels_total, level);
        }
        take_snapshot(store);
        context_pop(store);

        if (index) {
            bdd_temp.child0 = 1;
            store->bdd_data[bdd_temp.id] = bdd_temp;
            context_append(store, "Connect child 0 to <s>T</s> (is item %lld)", numbers[0]);
            take_snapshot(store);
            context_pop(store);
        } else {
            bdd_temp.child0 = 0;
            store->bdd_data[bdd_temp.id] = bdd_temp;
            context_append(store, "Do not connect child 0, empty list");
            take_snapshot(store);
            context_pop(store);
        }
        if (index < numbers.size) {
            bdd_temp.child1 = 1;
            store->bdd_data[bdd_temp.id] = bdd_temp;
            context_append(store, "Connect child 1 to <s>T</s> (is item %lld)", numbers[index]);
            take_snapshot(store);
            context_pop(store);
        } else {
            bdd_temp.child1 = 0;
            store->bdd_data[bdd_temp.id] = bdd_temp;
            context_append(store, "Do not connect child 1, empty list");
            take_snapshot(store);
            context_pop(store);
        }

        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    } else {
        // The general case.
        s64 index = _partition_along_bit(&numbers, level);
        auto lst0 = array_subarray(numbers, 0, index);
        auto lst1 = array_subarray(numbers, index, numbers.size);
        auto levels_sub = array_subarray(levels, 1, levels.size);

        context_append(store, "Splitting at bit %hhd into sublists of length %lld and %lld. The child 0 sublist is ", level, lst0.size, lst1.size);
        if (base != 2) {
            context_amend_list_base(store, lst0, 2, levels_total, level);
            context_amend(store, " (i.e. ");
            context_amend_list_base(store, lst0, base);
            context_amend(store, "), the child 1 sublist is ");
            context_amend_list_base(store, lst1, 2, levels_total, level);
            context_amend(store, " (i.e. ");
            context_amend_list_base(store, lst1, base);
            context_amend(store, ")");
        } else {
            context_amend_list_base(store, lst0, 2, levels_total, level);
            context_amend(store, ", the child 1 sublist is ");
            context_amend_list_base(store, lst1, 2, levels_total, level);
        }

        if (lst0.size and lst1.size) {
            bdd_name_amend_list_base(store, lst0, base);
            bdd_temp.child0 = bdd_create(store, {0, 0, (u8)(levels.size-1), Bdd::TEMPORARY});
            bdd_name_amend_list_base(store, lst1, base);
            bdd_temp.child1 = bdd_create(store, {0, 0, (u8)(levels.size-1), Bdd::TEMPORARY});
            store->bdd_data[bdd_temp.id] = bdd_temp;

            take_snapshot(store);
            context_pop(store);
        
            bdd_temp.child0 = bdd_from_list_stepwise(store, lst0, levels_sub, base, bdd_temp.child0, levels_total);
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);
            --store->snapshot_cur_node.size;

            bdd_temp.child1 = bdd_from_list_stepwise(store, lst1, levels_sub, base, bdd_temp.child1, levels_total);
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);
            --store->snapshot_cur_node.size;
        } else if (lst0.size) {
            bdd_name_amend_list_base(store, lst0, base);
            bdd_temp.child0 = bdd_create(store, {0, 0, (u8)(levels.size-1), Bdd::TEMPORARY});
            bdd_temp.child1 = 0;
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_append(store, "Child 1 is empty, not connected");
            take_snapshot(store);
            context_pop(store);
            context_pop(store);

            bdd_temp.child0 = bdd_from_list_stepwise(store, lst0, levels_sub, base, bdd_temp.child0, levels_total);
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);
            --store->snapshot_cur_node.size;
        } else if (lst1.size) {
            bdd_name_amend_list_base(store, lst1, base);
            bdd_temp.child0 = 0;
            bdd_temp.child1 = bdd_create(store, {0, 0, (u8)(levels.size-1), Bdd::TEMPORARY});
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_append(store, "Child 0 is empty, not connected");
            take_snapshot(store);
            context_pop(store);
            context_pop(store);

            bdd_temp.child1 = bdd_from_list_stepwise(store, lst1, levels_sub, base, bdd_temp.child1, levels_total);
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);
            --store->snapshot_cur_node.size;
        } else {
            assert_false;
        }

        context_pop(store);
        bdd_final = bdd_finalize(store, bdd_temp);
    }

    store->snapshot_cur_node[store->snapshot_cur_node.size-1] = bdd_final;
    if (bdd == (u32)-1) {
        store->snapshot_parents[store->snapshot_parents.size-1] = bdd_final;
        --store->snapshot_cur_node.size;
        context_append(store, "Done.");
        take_snapshot(store);
        context_pop(store);
        context_pop(store);
    }
    return bdd_final;
}

// Now comes the part responsible for parsing formulae and dealing with them. This represents an
// expression.
struct Formula {
    enum Type: u8 {
        NONE, VAR, NEG, OR, AND, XOR, IMPL_R, IMPL_L, IMPL_RL, ASSIGN,

        _PAREN_L, _PAREN_R // The parentheses are not used in a formula, but some of the functions dealing with operators want to talk about them for convenience
    };
    static constexpr char const* type_names[] = {
        "", "", "~", "|", "&", "^", "->", "<-", "<->", "=", "(", ")"
    };
    static constexpr char type_names_bdd[] = "..~+-^><=.";
    
    u8 type;
    s64 id;
    union {
        s64 var; // type == VAR
        s64 arg; // type == NEG
        struct { s64 arg_l, arg_r; }; // else
    };
    u64 hash; // Used for some of the simplifications
};
constexpr char const* Formula::type_names[];
constexpr char Formula::type_names_bdd[];

// Stores the state for the parser. Note that there are two types of ids here: Formulae and
// variables. The former index into formulae, the latter into vars and vars_formula.
struct Formula_store {
    Array_dyn<Formula> formulae; // All the formulae which are currently in use. Immutable. The first two are the constants 0 and 1. Also formulae[i].id == i.
    Array_dyn<s64> statements; // The list of statements, i.e. top-level formulae
    Array_dyn<s64> vars; // Indices into the var_names array with a dummy at the end
    Array_dyn<u8> var_names; // Data for the names of variables
    Array_dyn<s64> vars_formula; // One entry for each variable, either -1 for normal variables, or the index of the subformula, for variables representing subformulae.
    
    Array_dyn<s64> order_var; // The order of the variables for the purpose of bdd levels
    
    Array_dyn<s64> parser_ids; // identifier stack for the shunting-yard algorithm
    Array_dyn<u8> parser_ops; // operator stack for the shunting-yard algorithm
    s64 parser_diff = 0; // Difference in size of parser_ids and parser_ops
    
    Array_t<u8> str; // The string we are currently parsing
    s64 str_i, str_row, str_col, str_char; // Current position in the string

    bool error_flag; // A flag allowing errors to propagate upwards
};

Formula_store global_formula_store;

// splitmix64, written by Sebastiano Vigna
static inline uint64_t splitmix64(uint64_t index) {
    uint64_t z = (index + 0x9e3779b97f4a7c15ull);
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ull;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebull;
    return z ^ (z >> 31);
}

bool _is_operator_commutative(u8 type) {
    switch (type) {
    case Formula::OR:
    case Formula::AND:
    case Formula::XOR:
    case Formula::IMPL_RL:
        return true;
    default:
        return false;
    }
}

// Create a new formula and store it. Returns the id of the new formula.
s64 formula_create(Formula_store* store, u8 type, s64 arg0 = 0, s64 arg1 = 0) {
    Formula f;
    f.type = type;
    f.id = store->formulae.size;

    // Note on formula hashes: To facilitate simplifications, we want to detect whether two formulae
    // are equivalent. This is obviously too difficult in general, but I can do a simple syntatical
    // analysis by computing hashes based on the terms involved. As a neat trick I also compute the
    // hashes in a commutative fashion for the commutative operators, so that the order of their
    // arguments does not matter.
    
    if (type == Formula::NONE) {
        assert(false);
    } else if (type == Formula::NEG) {
        f.arg = arg0;
        f.hash = splitmix64(store->formulae[f.arg].hash ^ f.type);
    } else if (type == Formula::VAR) {
        f.var = arg0;
        f.hash = splitmix64(f.var ^ f.type);
    } else {
        f.arg_l = arg0;
        f.arg_r = arg1;
        u64 h0 = store->formulae[arg0].hash;
        u64 h1 = store->formulae[arg1].hash;
        if (_is_operator_commutative(type)) {
            f.hash = splitmix64(h0 ^ h1 ^ type);
        } else if (type == Formula::IMPL_L) {
            f.hash = splitmix64(splitmix64(h1 ^ Formula::IMPL_R) ^ h0);
        } else {
            f.hash = splitmix64(splitmix64(h0 ^ type) ^ h1);
        }
    }
    array_push_back(&store->formulae, f);
    return f.id;
}

// Initialise/reset a Formula_store.
void formula_store_init(Formula_store* store) {
    store->formulae.size = 0;
    store->statements.size = 0;
    store->vars.size = 0;
    store->var_names.size = 0;
    store->vars_formula.size = 0;
    store->order_var.size = 0;
    store->parser_ids.size = 0;
    store->parser_ops.size = 0;
    store->parser_diff = 0;
    
    array_printf(&store->var_names, "01");
    array_append(&store->vars, {0ll, 1ll, 2ll});
    formula_create(store, Formula::VAR, 0);
    formula_create(store, Formula::VAR, 1);
    
    store->error_flag = false;
    store->str_i = 0;
    store->str_row = 0;
    store->str_col = 0;
    store->str_char = 0;
}

bool _is_operator(u32 c) {
    switch (c) {
    case '|':
    case '&':
    case '~':
    case '^':
    case '=':
    case '<':
    case '>':
    case '-':
    case '(':
    case ')':
    case '+':
    case '%':
    case '/':
    case '?':
    case '!':
    case '#':
    case '\\':
    case ',':
    case U'∪':
    case U'∩':
    case U'¬':
    case U'∨':
    case U'∧':
    case U'←':
    case U'→':
    case U'↔':
    case U'⊕':
        return true;
    default:
        return false;
    }
}

bool _is_operator_flush(u32 c) {
    switch (c) {
    case '~':
    case '(':
    case ')':
    case U'¬':
        return true;
    default:
        return false;
    }
}

bool _is_space(u32 c) {
    return c == ' ' or c == '\t';
}

s64 _get_digit(u32 c) {
    switch (c) {
    case '0': return 0;
    case '1': return 1;
    case '2': return 2;
    case '3': return 3;
    case '4': return 4;
    case '5': return 5;
    case '6': return 6;
    case '7': return 7;
    case '8': return 8;
    case '9': return 9;
    case U'₀': return 0;
    case U'₁': return 1;
    case U'₂': return 2;
    case U'₃': return 3;
    case U'₄': return 4;
    case U'₅': return 5;
    case U'₆': return 6;
    case U'₇': return 7;
    case U'₈': return 8;
    case U'₉': return 9;
    default: return -1;
    }
}

// Decode the first unicode codepoint in buf, return the number of bytes it is long. The result value of the codepoint is written into c_out.
s64 helper_decode_utf8(Array_t<u8> buf, u32* c_out = nullptr) {
    static bool warning_was_given;
    
    u32 c = buf[0];
    s64 c_bytes = c&128 ? c&64 ? c&32 ? c&16 ? 4 : 3 : 2 : -1 : 1;
    if (buf.size < c_bytes) c_bytes = -1;
    
    if (c_bytes == 1) {
        // nothing
    } else if (c_bytes == 2) {
        c = (buf[0]&0x1f) << 6 | (buf[1]&0x3f);
    } else if (c_bytes == 3) {
        c = (buf[0]&0xf) << 12 | (buf[1]&0x3f) << 6 | (buf[2]&0x3f);
    } else if (c_bytes == 4) {
        c = (buf[0]&0x7) << 18 | (buf[1]&0x3f) << 12 | (buf[2]&0x3f) << 6 | (buf[3]&0x3f);
    } else {
        if (not warning_was_given) {
            fprintf(stderr, "Warning: encountered invalid utf-8 sequence (this warning will not show again)\n");
            warning_was_given = true;
        }
        assert(false);
        c = '?';
        c_bytes = 1;
    }
    if (c_out) *c_out = c;
    return c_bytes;
}

// Read the next token from the input and return its characters.
Array_t<u8> formula_token_pop(Formula_store* store) {
    if (store->str_i >= store->str.size) return {};

    // Note that identifiers consist of non-numeric, non-operator characters.
    
    s64 state = 0;
    s64 beg = 0, end = 0;
    while (store->str_i <= store->str.size) {
        u32 c;
        s64 bytes;

        if (store->str_i < store->str.size) {
            auto suffix = array_subarray(store->str, store->str_i, store->str.size);
            bytes = helper_decode_utf8(suffix, &c);
        } else {
            c = 0;
            bytes = 1;
        }
        
        if (state == 0) {
            if (_is_space(c))  {
                // nothing
            } else if (_get_digit(c) != -1) {
                beg = store->str_i;
                state = 1;
            } else if (c == '\n' or c == ';') {
                beg = store->str_i;
                end = store->str_i+1;
                state = 2;
            } else if (c == 0) {
                return {};
            } else if (_is_operator_flush(c)) {
                beg = store->str_i;
                end = store->str_i + bytes;
                store->str_i += bytes;
                ++store->str_char;
                break;
            } else if (_is_operator(c)) {
                beg = store->str_i;
                state = 4;
            } else {
                beg = store->str_i;
                state = 3;
            }
        } else if (state == 1) {
            if (_get_digit(c) != -1) {
                // nothing
            } else if (_is_operator(c) or _is_space(c) or c == ';' or c == '\n' or c == 0) {
                end = store->str_i;
                break;
            } else {
                ui_error_report("Error: Invalid token, unexpected char '%c' (codepoint %d) in number", (u8)c, c);
                store->error_flag = true;
                return {};
            }
        } else if (state == 2) {
            if (_is_space(c) or c == '\n' or c == ';') {
                // nothing
            } else {
                break;
            }
        } else if (state == 3) {
            if (_is_space(c) or _is_operator(c) or c == 0 or c == '\n' or c == ';') {
                end = store->str_i;
                break;
            } else {
                // nothing
            }
        } else if (state == 4) {
            if (not _is_operator(c) or _is_operator_flush(c)) {
                end = store->str_i;
                break;
            } else {
                // nothing
            }
        } else {
            assert(false);
        }

        store->str_i += bytes;
        ++store->str_char;
        if (c == '\n') {
            store->str_col = 0;
            ++store->str_row;
        } else {
            ++store->str_col;
        }
    }

    auto arr = array_subarray(store->str, beg, end);
    if (state == 1) {
        if (arr.size != 1 or arr[0] >= '2') {
            ui_error_report("Error: Invalid token, only 0 and 1 are allowed as constants");
            store->error_flag = true;
            return {};
        }
    }
    return arr;
}

u8 _get_operator_type(Array_t<u8> op) {
    if (array_equal_str(op, "~")) return Formula::NEG;
    if (array_equal_str(op, "|")) return Formula::OR;
    if (array_equal_str(op, "&")) return Formula::AND;
    if (array_equal_str(op, "^")) return Formula::XOR;
    if (array_equal_str(op, "->")) return Formula::IMPL_R;
    if (array_equal_str(op, "=>")) return Formula::IMPL_R;
    if (array_equal_str(op, "<-")) return Formula::IMPL_L;
    if (array_equal_str(op, "<=")) return Formula::IMPL_L;
    if (array_equal_str(op, "<->")) return Formula::IMPL_RL;
    if (array_equal_str(op, "<=>")) return Formula::IMPL_RL;

    if (array_equal_str(op, u8"∪")) return Formula::OR;
    if (array_equal_str(op, u8"∩")) return Formula::AND;
    if (array_equal_str(op, u8"¬")) return Formula::NEG;
    if (array_equal_str(op, u8"∨")) return Formula::OR;
    if (array_equal_str(op, u8"∧")) return Formula::AND;
    if (array_equal_str(op, u8"←")) return Formula::IMPL_L;
    if (array_equal_str(op, u8"→")) return Formula::IMPL_R;
    if (array_equal_str(op, u8"↔")) return Formula::IMPL_RL;
    if (array_equal_str(op, u8"⊕")) return Formula::XOR;
    
    if (array_equal_str(op, "=")) return Formula::ASSIGN;
    if (array_equal_str(op, "(")) return Formula::_PAREN_L;
    if (array_equal_str(op, ")")) return Formula::_PAREN_R;
    return Formula::NONE;
}

s64 _get_operator_precedence(u8 op) {
    // Only return even precedences here, to allow the other code to add 1 to deal with
    // associativity.
    switch (op) {
    case Formula::_PAREN_L: return 16;
    case Formula::_PAREN_R: return 16;
    case Formula::NEG:      return 14;
    case Formula::AND:      return 12;
    case Formula::OR:       return 10;
    case Formula::IMPL_R:   return  8;
    case Formula::IMPL_L:   return  8;
    case Formula::XOR:      return  6;
    case Formula::IMPL_RL:  return  4;
    case Formula::ASSIGN:   return  2;
    case Formula::NONE:     return  0;
    default: assert(false); return  0;
    }
}

// Now we get a bunch of functions that generate a human-readable version of a formula. These all
// work the same, but the details of the output format differ.

// Print to stdout for debugging.
void formula_print(Formula_store* store, s64 f_id, s64 parent_prec = 0) {
    Formula f = store->formulae[f_id];
    if (f.type == Formula::NONE) {
        printf(".");
    } else if (f.type == Formula::VAR) {
        auto name = array_subarray(store->var_names, store->vars[f.var], store->vars[f.var+1]);
        fwrite(name.data, name.size, 1, stdout);
    } else if (f.type == Formula::NEG) {
        printf("~");
        formula_print(store, f.arg, _get_operator_precedence(f.type));
    } else {
        bool paren = _get_operator_precedence(f.type) <= parent_prec;
        bool right_assoc = f.type == Formula::IMPL_R;
        if (paren) printf("(");
        formula_print(store, f.arg_l, f.type + 1-right_assoc);
        printf("%s", Formula::type_names[f.type]);
        formula_print(store, f.arg_r, f.type +   right_assoc);
        if (paren) printf(")");
    }
}

// Like context_amend
void context_amend_formula_var(Bdd_store* store, Formula_store* fstore, s64 var) {
    auto name = array_subarray(fstore->var_names, fstore->vars[var], fstore->vars[var+1]);
    for (u8 c: name) {
        if (var > 1 and _get_digit(c) != -1) {
            u8 cc[] = {0xe2, 0x82, (u8)(0x80 + _get_digit(c))};
            context_amend(store, {cc, 3});
        } else {
            context_amend(store, {&c, 1});
        }
    }
}
void _context_amend_formula_helper(Bdd_store* store, Formula_store* fstore, s64 f_id, s64 parent_prec = 0) {
    Formula f = fstore->formulae[f_id];
    if (f.type == Formula::NONE) {
        assert(false);
    } else if (f.type == Formula::VAR) {
        context_amend_formula_var(store, fstore, f.var);
    } else if (f.type == Formula::NEG) {
        auto cc = opengl_bddlabel_char_utf8(Formula::type_names_bdd[f.type]);
        context_amend(store, cc);
        _context_amend_formula_helper(store, fstore, f.arg, _get_operator_precedence(f.type));
    } else {
        bool paren = _get_operator_precedence(f.type) <= parent_prec;
        bool right_assoc = f.type == Formula::IMPL_R;
        if (paren) context_amend(store, "(");
        _context_amend_formula_helper(store, fstore, f.arg_l, f.type + 1-right_assoc);
        auto cc = opengl_bddlabel_char_utf8(Formula::type_names_bdd[f.type]);
        context_amend(store, cc);
        _context_amend_formula_helper(store, fstore, f.arg_r, f.type +   right_assoc);
        if (paren) context_amend(store, ")");
    }
}

void context_amend_formula(Bdd_store* store, Formula_store* fstore, s64 f_id, s64 parent_prec = 0) {
    context_amend(store, "<s>");
    _context_amend_formula_helper(store, fstore, f_id, parent_prec);
    context_amend(store, "</s>");
}

// Like bdd_name_amend_*
void bdd_name_amend_formula(Bdd_store* store, Formula_store* fstore, s64 f_id, s64 parent_prec = 0) {
    Formula f = fstore->formulae[f_id];
    if (f.type == Formula::NONE) {
        assert(false);
    } else if (f.type == Formula::VAR) {
        auto name = array_subarray(fstore->var_names, fstore->vars[f.var], fstore->vars[f.var+1]);
        for (u8 c: name) {
            u8 cc = f.id > 1 and '0' <= c and c <= '9' ? 128 | (c-'0'+22) : 128 | c;
            array_push_back(&store->name_data, cc);
        }
    } else if (f.type == Formula::NEG) {
        array_push_back(&store->name_data, (u8)(128 | Formula::type_names_bdd[f.type]));
        bdd_name_amend_formula(store, fstore, f.arg, _get_operator_precedence(f.type));
    } else {
        bool paren = _get_operator_precedence(f.type) <= parent_prec;
        bool right_assoc = f.type == Formula::IMPL_R;
        if (paren) array_push_back(&store->name_data, (u8)(128 | '('));
        bdd_name_amend_formula(store, fstore, f.arg_l, f.type + 1-right_assoc);
        array_push_back(&store->name_data, (u8)(128 | Formula::type_names_bdd[f.type]));
        bdd_name_amend_formula(store, fstore, f.arg_r, f.type +   right_assoc);
        if (paren) array_push_back(&store->name_data, (u8)(128 | ')'));
    }
}

// Read the variable name and create a new variable, or take an existing one, if it exists.
s64 formula_parse_var(Formula_store* store, Array_t<u8> tok) {
    // Replace subscripts with ASCII digits, so that a user can simply copy-paste the formulae when
    // printed with unicode characters.
    s64 var_names_size = store->var_names.size;
    for (s64 i = 0; i < tok.size;) {
        u32 c;
        s64 bytes = helper_decode_utf8(array_subarray(tok, i, tok.size), &c);
        if (_get_digit(c) != -1) {
            array_push_back(&store->var_names, (u8)('0' + _get_digit(c)));
        } else {
            array_append(&store->var_names, array_subarray(tok, i, i+bytes));
        }
        i += bytes;
    }
    auto name = array_subarray(store->var_names, var_names_size, store->var_names.size);

    // Try to find an existing variable with the same name
    // @Speed: This is quadratic, could be linear
    s64 var = -1;
    for (s64 i = 0; i+1 < store->vars.size; ++i) {
        if (array_equal(name, array_subarray(store->var_names, store->vars[i], store->vars[i+1]))) {
            var = i;
            break;
        }
    }
    
    if (var == -1) {
        var = store->vars.size-1;
        array_push_back(&store->vars, store->var_names.size);
    } else {
        store->var_names.size = var_names_size;
    }
    return var;
}

// Parse a single top-level statement and add it to store->statements
void formula_parse_statement(Formula_store* store) {
    // This is an operator precedence parser using the shunting-yard algorithm.
    
    auto pop_binop = [store]() {
        assert(store->parser_ids.size >= 2);
        u8 top_typ = store->parser_ops[store->parser_ops.size-1];
        s64 ss = store->parser_ids.size;
        store->parser_ids[ss-2] = formula_create(store, top_typ, store->parser_ids[ss-2], store->parser_ids[ss-1]);
        --store->parser_ids.size;
        --store->parser_ops.size;
    };
    auto pop_unop = [store]() {
        assert(store->parser_ids.size >= 1);
        assert(store->parser_ops[store->parser_ops.size-1] == Formula::NEG);
        s64 ss = store->parser_ids.size;
        store->parser_ids[ss-1] = formula_create(store, Formula::NEG, store->parser_ids[ss-1]);
        --store->parser_ops.size;
    };
    
    while (true) {
        auto tok = formula_token_pop(store);
        if (store->error_flag) return;

        u32 tok_0 = 0;
        if (tok.size != 0) helper_decode_utf8(tok, &tok_0);
        
        if (tok.size == 0 or tok_0 == '\n' or tok_0 == ';') {
            // End of statement, flush what we have and return

            // Check for empty statement
            if (store->parser_ids.size == 0) break;
            
            if (store->parser_diff == 0) {
                ui_error_report("Error: Unexpected end of statement");
                store->error_flag = true;
                return;
            }

            // Flush the operators on the stack
            while (true) {
                s64 s = store->parser_ops.size;
                if (not s) break;
                s64 top_typ = store->parser_ops[s-1];
                
                if (top_typ == Formula::NEG) {
                    ui_error_report("Error: Unexpected end of statement after unary '~'");
                    store->error_flag = true;
                    return;
                } else if (top_typ == Formula::_PAREN_L) {
                    ui_error_report("Error: Unexpected end of statement, mismatched '('");
                    store->error_flag = true;
                    return;
                } else {
                    pop_binop();
                }
            }

            // Clear the stacks and write the formula into statements
            assert(store->parser_ids.size <= 1);
            if (store->parser_ids.size == 1) {
                array_push_back(&store->statements, store->parser_ids[0]);
                --store->parser_ids.size;
                store->parser_diff = 0;
            }

            break;
        } else if (tok_0 == ')') {
            // Flush the stack until the last '('
            
            while (true) {
                s64 s = store->parser_ops.size;
                if (not s) {
                    ui_error_report("Error: Mismatched ')'");
                    store->error_flag = true;
                    return;
                }
                if (store->parser_ops[s-1] == Formula::_PAREN_L) {
                    --store->parser_ops.size;
                    break;
                }
                pop_binop();
            }

            while (store->parser_ops.size
                   and store->parser_ops[store->parser_ops.size-1] == Formula::NEG) {
                pop_unop();
            }
        } else if (_is_operator(tok_0)) {
            // Parse the operator
            u8 typ = _get_operator_type(tok);
            if (typ == Formula::NONE) {
                Array_dyn<u8> str;
                defer { array_free(&str); };
                array_printf(&str, "Error: Invalid operator '");
                array_append(&str, tok);
                array_push_back(&str, (u8)'\'');
                array_push_back(&str, (u8)0);
                ui_error_report((char*)str.data);
                store->error_flag = true;
                return;
            }

            // Flush operators with higher precedence from the stack
            s64 prec = _get_operator_precedence(typ);
            s64 right_assoc = typ == Formula::IMPL_R;
            while (true) {
                s64 s = store->parser_ops.size;
                if (not s) break;
                s64 top_typ = store->parser_ops[s-1];
                s64 top_prec = _get_operator_precedence(top_typ);

                if (prec > top_prec or (prec == top_prec and right_assoc)) break;
                if (store->parser_ops[s-1] == Formula::_PAREN_L) break;

                if (top_typ == Formula::NEG) {
                    if (typ == Formula::NEG) break;
                    ui_error_report("Error: Expected term after '~', got operator");
                    store->error_flag = true;
                    return;
                }

                pop_binop();
            }

            // Check whether this is all fine
            if (typ != Formula::_PAREN_L and typ != Formula::NEG) {
                if (store->parser_diff != 1) {
                    ui_error_report("Error: Expected identifier or constant, got operator");
                    store->error_flag = true;
                    return;
                }
                store->parser_diff = 0;
            }
            
            array_push_back(&store->parser_ops, typ);
        } else if (_get_digit(tok_0) != -1) {
            // Got a constant
            
            assert(tok_0 == '0' or tok_0 == '1');
            array_push_back(&store->parser_ids, (s64)(tok_0 - '0'));

            if (store->parser_diff != 0) {
                ui_error_report("Error: Expected operator, got constant");
                store->error_flag = true;
                return;
            }
            store->parser_diff = 1;

            while (store->parser_ops.size
                   and store->parser_ops[store->parser_ops.size-1] == Formula::NEG) {
                pop_unop();
            }
        } else {
            // Else, it must be an identifier
            
            s64 var = formula_parse_var(store, tok);
            array_push_back(&store->parser_ids, formula_create(store, Formula::VAR, var));

            if (store->parser_diff != 0) {
                ui_error_report("Error: Expected operator, got identifier");
                store->error_flag = true;
                return;
            }
            store->parser_diff = 1;

            while (store->parser_ops.size
                   and store->parser_ops[store->parser_ops.size-1] == Formula::NEG) {
                pop_unop();
            }
        }
    }
}

// Parse the whole formula written in str, with variable order order.
s64 formula_parse(Formula_store* store, Array_t<u8> str, Array_t<u8> order) {
    formula_store_init(store);
    store->str = str;

    while (store->str_i < store->str.size) {
        formula_parse_statement(store);
        if (store->error_flag) return -1;
    }

    // Recursion without using std::function. C++ is really showing its greatness here, after all,
    // why would anyone want to use recursion in a lambda?
    auto check_no_assign = [store](s64 f_id) {
        auto _rec = [store](s64 f_id, auto& self) mutable {
            Formula f = store->formulae[f_id];
            if (f.type == Formula::ASSIGN) {
                ui_error_report("Error: Invalid assignment in expression (use '<->' for equality)");
                store->error_flag = true;
                return;
            } else if (f.type == Formula::NEG) {
                self(f.arg, self);
            } else if (f.type == Formula::VAR) {
                // nothing
            } else if (f.type == Formula::NONE) {
                assert(false);
            } else {
                self(f.arg_l, self);
                if (store->error_flag) return;
                self(f.arg_r, self);
            }
        };
        return _rec(f_id, _rec);
    };

    // Determine what variables are responsible for subformulae, i.e. perform the assignments
    array_resize(&store->vars_formula, store->vars.size);
    memset(store->vars_formula.data, -1, store->vars_formula.size * sizeof(store->vars_formula[0]));
    store->vars_formula[0] = 0;
    store->vars_formula[1] = 1;

    {s64 j = 0;
    for (s64 i = 0; i < store->statements.size; ++i) {
        Formula f = store->formulae[store->statements[i]];
        if (f.type == Formula::ASSIGN) {
            Formula f_l = store->formulae[f.arg_l];
            if (f_l.type != Formula::VAR) {
                ui_error_report("Error: Cannot assign to non-variable");
                store->error_flag = true;
                return -1;
            } else if (store->vars_formula[f_l.var] != -1) {
                ui_error_report("Error: Cannot assign to assigned variable or constant");
                store->error_flag = true;
                return -1;
            }
            check_no_assign(f.arg_r);

            store->vars_formula[f_l.var] = f.arg_r;
        } else {
            check_no_assign(f.id);
            store->statements[j++] = f.id;
        }
    }
    store->statements.size = j;}

    if (store->statements.size == 0) {
        ui_error_report("Error: Empty formula");
        store->error_flag = true;
        return -1;
    }

    // Parse the variable order
    store->str = order;
    store->str_i = 0;
    store->str_row = 0;
    store->str_col = 0;
    store->str_char = 0;
    bool first = true;
    while (true) {
        auto tok = formula_token_pop(store);

        if (first and array_equal_str(tok, "auto")) {
            break;
        }
        first = false;
        
        if (tok.size == 0) break;

        // Ignore operators
        u32 tok_0; helper_decode_utf8(tok, &tok_0);
        if (_is_operator(tok_0)) continue;
        
        if (_get_digit(tok_0) != -1) {
            ui_error_report("Error: Unexpected number in variable order list, which should consist "
                "only of comma-separated variable names");
            return -1;
        }

        s64 var = formula_parse_var(store, tok);

        if (store->vars_formula[var] != -1) {
            ui_error_report("Error: Variable in variable order list is actually a subformula");
            return -1;
        }
        
        for (s64 i: store->order_var) {
            if (var == i) {
                ui_error_report("Error: Variable appears twice in variable order list");
                return -1;
            }
        }
        array_push_back(&store->order_var, var);
    }
    s64 first_to_sort = store->order_var.size;
    
    for (Formula f: store->formulae) {
        if (f.type != Formula::VAR) continue;
        if (store->vars_formula[f.var] != -1) continue;

        bool found = false;
        for (s64 i: store->order_var) {
            if (f.var == i) {
                found = true;
                break;
            }
        }

        if (not found) {
            array_push_back(&store->order_var, f.var);
        }
    }

    std::sort(store->order_var.begin() + first_to_sort, store->order_var.end(), [store](s64 a, s64 b) {
        auto a_name = array_subarray(store->var_names, store->vars[a], store->vars[a+1]);
        auto b_name = array_subarray(store->var_names, store->vars[b], store->vars[b+1]);

        s64 a_i = a_name.size;
        while (a_i > 0 and '0' <= a_name[a_i-1] and a_name[a_i-1] <= '9') --a_i;
        s64 b_i = b_name.size;
        while (b_i > 0 and '0' <= b_name[b_i-1] and b_name[b_i-1] <= '9') --b_i;

        for (s64 i = 0; i < a_i and i < b_i; ++i) {
            if (a_name[i] < b_name[i]) return true;
            if (a_name[i] > b_name[i]) return false;
        }
        if (a_i < b_i) return true;
        if (a_i > b_i) return false;
        
        s64 a_n = 0;
        for (s64 i = a_i; i < a_name.size; ++i) { a_n = a_n*10 + (a_name[i] - '0'); }
        s64 b_n = 0;
        for (s64 i = b_i; i < b_name.size; ++i) { b_n = b_n*10 + (b_name[i] - '0'); }

        return a_n < b_n;
    });

    for (s64 i = 1; i < store->statements.size; ++i) {
        store->statements[0] = formula_create(store, Formula::AND, store->statements[0], store->statements[i]);
    }
    store->statements.size = 1;
    
    return store->statements[0];
}

s64 formula_assign_var(Formula_store* store, s64 f_id, s64 var, bool value) {
    Formula f = store->formulae[f_id];
    if (f.type == Formula::NONE) {
        assert(false);
        return 0;
    } else if (f.type == Formula::VAR) {
        s64 sub = store->vars_formula[f.var];
        if (sub > 1) {
            s64 subs = formula_assign_var(store, sub, var, value);
            if (subs != sub) return subs;
        }

        return f.var == var ? value : f_id;
    } else if (f.type == Formula::NEG) {
        s64 f0 = formula_assign_var(store, f.arg, var, value);
        if (f0 != f.arg) {
            return formula_create(store, f.type, f0);
        } else {
            return f.id;
        }
    } else {
        s64 f_l = formula_assign_var(store, f.arg_l, var, value);
        s64 f_r = formula_assign_var(store, f.arg_r, var, value);
        if (f_l != f.arg_l or f_r != f.arg_r) {
            return formula_create(store, f.type, f_l, f_r);
        } else {
            return f.id;
        }
    }
}

s64 formula_simplify(Formula_store* store, s64 f_id) {
    Formula f = store->formulae[f_id];
    if (f.type == Formula::NONE) {
        assert(false);
        return 0;
    } else if (f.type == Formula::VAR) {
        return f_id;
    } else if (f.type == Formula::NEG) {
        Formula f0 = store->formulae[formula_simplify(store, f.arg)];
        
        if (f0.type == Formula::NEG) {
            return f0.arg;
        } else if (f0.id <= 1) {
            return 1 - f0.id;
        } else if (f0.id == f.arg) {
            return f.id;
        } else {
            return formula_create(store, Formula::NEG, f0.id);
        }
    } else {
        Formula f0 = store->formulae[formula_simplify(store, f.arg_l)];
        Formula f1 = store->formulae[formula_simplify(store, f.arg_r)];

        auto create_simplified = [store](u8 type, s64 arg0 = 0, s64 arg1 = 0) {
            return formula_simplify(store, formula_create(store, type, arg0, arg1));
        };
        
        if (f.type == Formula::OR) {
            if (f0.id == 0) return f1.id;
            if (f1.id == 0) return f0.id;
            if (f0.id == 1) return 1;
            if (f1.id == 1) return 1;
            if (f0.hash == f1.hash) return f0.id;
            if (f0.type == Formula::NEG and store->formulae[f0.arg].hash == f1.hash) return 1;
            if (f1.type == Formula::NEG and store->formulae[f1.arg].hash == f0.hash) return 1;
        } else if (f.type == Formula::AND) {
            if (f0.id == 0) return 0;
            if (f1.id == 0) return 0;
            if (f0.id == 1) return f1.id;
            if (f1.id == 1) return f0.id;
            if (f0.hash == f1.hash) return f0.id;
            if (f0.type == Formula::NEG and store->formulae[f0.arg].hash == f1.hash) return 0;
            if (f1.type == Formula::NEG and store->formulae[f1.arg].hash == f0.hash) return 0;
        } else if (f.type == Formula::XOR) {
            if (f0.id == 0) return f1.id;
            if (f1.id == 0) return f0.id;
            if (f0.id == 1) return create_simplified(Formula::NEG, f1.id);
            if (f1.id == 1) return create_simplified(Formula::NEG, f0.id);
            if (f0.hash == f1.hash) return 0;
            if (f0.type == Formula::NEG and store->formulae[f0.arg].hash == f1.hash) return 1;
            if (f1.type == Formula::NEG and store->formulae[f1.arg].hash == f0.hash) return 1;
        } else if (f.type == Formula::IMPL_R) {
            if (f0.id == 0) return 1;
            if (f1.id == 0) return create_simplified(Formula::NEG, f0.id);
            if (f0.id == 1) return f1.id;
            if (f1.id == 1) return 1;
            if (f0.hash == f1.hash) return 1;
            if (f0.type == Formula::NEG and store->formulae[f0.arg].hash == f1.hash) return f1.id;
            if (f1.type == Formula::NEG and store->formulae[f1.arg].hash == f0.hash) return f1.id;
        } else if (f.type == Formula::IMPL_L) {
            if (f0.id == 0) return create_simplified(Formula::NEG, f1.id);
            if (f1.id == 0) return 1;
            if (f0.id == 1) return 1;
            if (f1.id == 1) return f0.id;
            if (f0.hash == f1.hash) return 1;
            if (f0.type == Formula::NEG and store->formulae[f0.arg].hash == f1.hash) return f0.id;
            if (f1.type == Formula::NEG and store->formulae[f1.arg].hash == f0.hash) return f0.id;
        } else if (f.type == Formula::IMPL_RL) {
            if (f0.id == 0) return create_simplified(Formula::NEG, f1.id);
            if (f1.id == 0) return create_simplified(Formula::NEG, f0.id);
            if (f0.id == 1) return f1.id;
            if (f1.id == 1) return f0.id;
            if (f0.hash == f1.hash) return 1;
            if (f0.type == Formula::NEG and store->formulae[f0.arg].hash == f1.hash) return 0;
            if (f1.type == Formula::NEG and store->formulae[f1.arg].hash == f0.hash) return 0;
            if (f0.type == Formula::NEG and f1.type == Formula::NEG) return formula_create(store, f.type, f0.arg, f1.arg);
        } else {
            assert(false);
        }
        
        if (f0.id == f.arg_l and f1.id == f.arg_r) return f.id;
        return formula_create(store, f.type, f0.id, f1.id);
    }    
}

u32 _bdd_from_formula_stepwise_helper(Bdd_store* store, Formula_store* fstore, s64 f_id, u32 bdd = -1) {
    Bdd bdd_temp;
    if (bdd == (u32)-1) {
        context_append(store, "Creating BDD from formula ");
        context_amend_formula(store, fstore, f_id);
        context_amend(store, " using variable order <s>");
        bool first = true;
        for (s64 i: fstore->order_var) {
            if (not first) context_amend(store, ", ");
            first = false;
            context_amend_formula_var(store, fstore, i);
        }
        context_amend(store, "</s>");

        bdd_name_amend_formula(store, fstore, f_id);
        
        bdd_temp = {0, 0, (u8)fstore->order_var.size, Bdd::TEMPORARY};
        bdd_create_inplace(store, &bdd_temp);
        array_push_back(&store->snapshot_parents, bdd_temp.id);
    } else {
        bdd_temp = store->bdd_data[bdd];
        context_append(store, "Processing subformula ");
        context_amend_formula(store, fstore, f_id);
    }

    array_push_back(&store->snapshot_cur_node, bdd_temp.id);
    take_snapshot(store);

    s64 f_id_simple = formula_simplify(fstore, f_id);
    if (f_id_simple != f_id) {
        context_append(store, "Simplify formula into ");
        context_amend_formula(store, fstore, f_id_simple);
        bdd_name_amend_formula(store, fstore, f_id_simple);
        bdd_name_assign(store, &bdd_temp);
        take_snapshot(store);
        context_pop(store);
        f_id = f_id_simple;
    }

    u32 bdd_final;
    if (f_id == 0) {
        bdd_final = 0;
        context_pop(store);
        context_append(store, "Formula is unsatisfiable, no connection");
    } else if (f_id == 1) {
        bdd_final = 1;
        context_pop(store);
        context_append(store, "Formula is tautology, connect to <s>T</s>");
    } else {
        s64 order_level = fstore->order_var.size - bdd_temp.level;
        s64 f0_id = formula_assign_var(fstore, f_id, fstore->order_var[order_level], 0);
        s64 f1_id = formula_assign_var(fstore, f_id, fstore->order_var[order_level], 1);

        if (f0_id == f1_id) {
            context_append(store, "Splitting at variable <s>");
            context_amend_formula_var(store, fstore, fstore->order_var[order_level]);
            context_amend(store, "</s> (level %lld), which leaves the formula unchanged", order_level);
        
            --bdd_temp.level;
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);

            bdd_final = _bdd_from_formula_stepwise_helper(store, fstore, f0_id, bdd_temp.id);
            context_pop(store);
            --store->snapshot_cur_node.size;
        } else if (bdd_temp.level == 1) {
            f0_id = formula_simplify(fstore, f0_id);
            f1_id = formula_simplify(fstore, f1_id);

            // We know that at this point the formulae have to be simplified to either 0 or 1
            assert(f0_id <= 1 and f1_id <= 1);

            context_append(store, "Node at last level, direct connections. Connect child 0 to ");
            context_amend_bdd(store, f0_id);
            context_amend(store, ", child 1 to ");
            context_amend_bdd(store, f1_id);
            
            bdd_temp.child0 = f0_id;
            bdd_temp.child1 = f1_id;
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);

            context_pop(store);
            bdd_final = bdd_finalize(store, bdd_temp);
        } else {
            context_append(store, "Splitting at variable <s>");
            context_amend_formula_var(store, fstore, fstore->order_var[order_level]);
            context_amend(store, "</s> (level %lld) into subformulae. The child 0 subformula is ", order_level);
            context_amend_formula(store, fstore, f0_id);
            context_amend(store, ", the child 1 subformula is ");
            context_amend_formula(store, fstore, f1_id);
        
            bdd_name_amend_formula(store, fstore, f0_id);
            bdd_temp.child0 = bdd_create(store, {0, 0, (u8)(bdd_temp.level-1), Bdd::TEMPORARY});
            bdd_name_amend_formula(store, fstore, f1_id);
            bdd_temp.child1 = bdd_create(store, {0, 0, (u8)(bdd_temp.level-1), Bdd::TEMPORARY});

            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);

            bdd_temp.child0 = _bdd_from_formula_stepwise_helper(store, fstore, f0_id, bdd_temp.child0);
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);
            --store->snapshot_cur_node.size;
        
            bdd_temp.child1 = _bdd_from_formula_stepwise_helper(store, fstore, f1_id, bdd_temp.child1);
            store->bdd_data[bdd_temp.id] = bdd_temp;
            take_snapshot(store);
            context_pop(store);
            --store->snapshot_cur_node.size;

            context_pop(store);
            bdd_final = bdd_finalize(store, bdd_temp);
        }
    }
    
    store->snapshot_cur_node[store->snapshot_cur_node.size-1] = bdd_final;
    if (bdd == (u32)-1) {
        store->snapshot_parents[store->snapshot_parents.size-1] = bdd_final;
        --store->snapshot_cur_node.size;
        context_append(store, "Done.");
        take_snapshot(store);
        context_pop(store);
        context_pop(store);
    }
    return bdd_final;
}

u32 bdd_from_formula_stepwise(Bdd_store* store, Formula_store* fstore, s64 f_id) {
    // @Cleanup: Inline this call
    return _bdd_from_formula_stepwise_helper(store, fstore, f_id);
}

// Now we get into the layout part.

// Yeah, having a proper 2D-vector class would have been the right call in retrospect.
struct Pos {
    float x = 0.f, y = 0.f;
};

// Affine combination of p0, p1 with parameter t. You know, lerping.
Pos _pos_mix(Pos p0, Pos p1, float t) {
    Pos r;
    r.x = (1.f-t) * p0.x + t * p1.x;
    r.y = (1.f-t) * p0.y + t * p1.y;
    return r;
};

// Its like Pos, but with id!
struct Pos_id {
    enum Flags: u8 {
        FIXED = 1,       // These nodes are already there from the last layout, so do not change their relative position 
        INITIALIZED = 2, // Whether the node as been assigned a position already
        REPOSITION = 4,  // In the formula animation, a node may change levels. This flag marks a node which just did.
    };
    float x, y;
    u32 id;
    u8 flags;
};

// Representing an 'intermediate' node. These are created for each edge spanning multiple layers.
struct Inter {
    u32 parent, child, id;
};

// A single edge from one (non-intermediate) node to another (non-intermediate) node. This is a
// sequence of (stright) lines.
struct Edge {
    // Type of edge. Note that each edge is uniquely identified by the parent id and its type.
    enum Type: u8 {
        INVALID = 0, CHILD0, CHILD1
    };
    
    u64 offset; // offset into the edge_data array
    u8 type;
    u32 from, to; // offsets, not ids
};

// Contains all the data necessary to store layouts, i.e. 2D-embeddings of the graph. A specific
// layout always depends on a set of nodes to lay out, and the order these nodes are stored in. In
// particular, this means that some of the arrays below only make sense if you have the
// corresponding bdds that were used to create the layout (so the ones of the snapshot). In the
// following I call this array of bdds just bdds.
//
// Also note that due to creating artificial nodes for intermediates, the internal arrays are longer
// than the original.
struct Bdd_layout {
    Array_dyn<Pos_id> bdd_pos; // The position of each bdd, same order as in bdds: bdds[i].id == bdd_pos[i].id
    Array_dyn<Edge> edges; // The set of edges. No particular order
    Array_dyn<Pos> edge_data; // Position data for the nodes of an edge
    Array_dyn<Inter> inters; // Set of intermediate nodes. Parents come before their children.
    u32 id_max; // The next id guaranteed to be free (see note on id generation below)
    u32 id_max_bdd; // The first id not belonging to a bdd (see note on id generation below)
};

// Initialises the layout. id_max should be the largest id any bdd can have, plus one.
void layout_init(Bdd_layout* layout, u32 id_max) {
    assert(layout);
    
    std::memset(layout, 0, sizeof(Bdd_layout));
    array_push_back(&layout->edges, Edge {0, Edge::INVALID, (u32)-1, (u32)-1});

    layout->id_max = id_max;
    layout->id_max_bdd = id_max;
}

// Remap the ids. When we are generating a layout using one from a previous stepwise algorithm as
// basis, the ids assigned to the artificial nodes (the intermediates) and the new bdds generated in
// the current algorithm may collide. To prevent this, we map the ids of artificial nodes into a
// safe range.
void layout_set_id_max(Bdd_layout* layout, u32 id_max) {
    assert(id_max >= layout->id_max_bdd);
    u32 id_diff = id_max - layout->id_max_bdd;

    u32 id_max_new = id_max;
    for (Pos_id& i: layout->bdd_pos) {
        if (layout->id_max_bdd <= i.id) i.id += id_diff;
        if (id_max_new <= i.id) id_max_new = i.id+1;
    }
    for (Inter& i: layout->inters) {
        if (layout->id_max_bdd <= i.id)     i.id     += id_diff;
        if (layout->id_max_bdd <= i.parent) i.parent += id_diff;
        if (layout->id_max_bdd <= i.child)  i.child  += id_diff;
        if (id_max_new <= i.id)     id_max_new = i.id+1;
        if (id_max_new <= i.parent) id_max_new = i.parent+1;
        if (id_max_new <= i.child)  id_max_new = i.child+1;
    }

    layout->id_max = id_max_new;
    layout->id_max_bdd = id_max;
}

// Do a deep copy of b into a.
void layout_copy(Bdd_layout* a, Bdd_layout b) {
    a->bdd_pos.size = 0;
    a->edges.size = 0;
    a->edge_data.size = 0;
    a->inters.size = 0;
    array_append(&a->bdd_pos,   b.bdd_pos);
    array_append(&a->edges,     b.edges);
    array_append(&a->edge_data, b.edge_data);
    array_append(&a->inters,    b.inters);
    a->id_max = b.id_max;
    a->id_max_bdd = b.id_max_bdd;
};

// Acceleration data structure. Used to store all edges between both real and artificial nodes in an
// array, to update their forces faster.
struct Child_edge {
    u32 parent, child;
};

// layout_graph is a function I have optimised a bit, as that is the main performance bottleneck. As
// part of that optimisation I introduced this struct to reuse memory. To be thourough, one would
// have to remove dynamic allocation from take_snapshot as well.
struct Layout_memory {
    Array_dyn<Bdd> bdds;
    Array_dyn<u32> id_map;
    Array_dyn<float> vel;
    Array_dyn<float> vel_buf;
    Array_dyn<Pos_id> pos;
    Array_dyn<float> pos_orig;
    Array_dyn<u64> pos_inter_flag;
    Array_dyn<Child_edge> pos_children;
    Array_dyn<u32> pos_map;
    Array_dyn<Pos> edge_data_new;
    Array_dyn<Pos> i_data;
};

// This little function is at the heart of the layout algorithm. It takes a list of bdds to lay out,
// bdds_, and writes the result into layout. This builds incrementally upon the data already present
// in layout. memory is used to avoid memory allocation. iter_max determines how many iterations to
// run the force-based simulation for.
void layout_graph(Array_t<Bdd> bdds_, Bdd_layout* layout, Layout_memory* memory, s64 iter_max) {
    // We will need to add artificial nodes, so create an array for that.
    Array_dyn<Bdd> bdds = memory->bdds;
    defer { memory->bdds = bdds; };
    bdds.size = 0;
    array_append(&bdds, bdds_);

    // Note on id generation:
    //  There are two main ways to refer to bdds: 1. Their id.  2. An offset into the bdds array.
    // 2 -> 1 goes via bdds[i].id and mapping 1 -> 2 is id_map[i].
    //  Actually, there is also 3, an offset into the bdds array of the previous layout. Mapping
    // 3 -> 1 is done by taking layout->bdd_pos[i].id. 1 -> 3 is not needed.
    //  When we generate a new id for an artificial node, we take one from the end of the current
    // id-space. That ensures that (during the layouting for a single stepwise algorithm) no
    // artificial ids can collide with bdd ids. However, when reusing the layout for a new setpwise
    // algorithm, we need to update the artificial ids to take new bdds into account.
    //  Also note that all these mappings are done via arrays. That is perfectly fine for this
    // use-case, but does not handle the general case well. Still, I do not expect to deal with more
    // than a few thousand nodes at a time, and the code will not be able to handle that regardless.
    Array_dyn<u32> id_map = memory->id_map;
    defer { memory->id_map = id_map; };
    //id_map.size = 0;
    array_reserve(&id_map, layout->id_max + 10);
    id_map.size = layout->id_max;
    memset(id_map.data, -1, id_map.size * sizeof(u32));
    
    for (s64 i = 0; i < bdds.size; ++i) {
        id_map[bdds[i].id] = i;
    }

    // See which one of the old intermediates are still valid and add them. Delete the rest.
    {s64 j = 0;
    for (s64 i_it = 0; i_it < layout->inters.size; ++i_it) {
        Inter i = layout->inters[i_it];
        if (id_map[i.parent] == (u32)-1) continue;
        Bdd* i_bdd = &bdds[id_map[i.parent]];
        if (i_bdd->child0 == i.child and i_bdd->level > bdds[id_map[i.child]].level+1) {
            Bdd inter = {i_bdd->child0, i_bdd->child0, (u8)(i_bdd->level-1), Bdd::INTERMEDIATE, i.id};
            i_bdd->child0 = i.id;
            id_map[i.id] = bdds.size;
            array_push_back(&bdds, inter);
            layout->inters[j++] = i;
            if (i_bdd->child1 == i.child) {
                i_bdd->child1 = i_bdd->child0;
                continue;
            }
        }
        if (i_bdd->child1 == i.child and i_bdd->level > bdds[id_map[i.child]].level+1) {
            Bdd inter = {i_bdd->child1, i_bdd->child1, (u8)(i_bdd->level-1), Bdd::INTERMEDIATE, i.id};
            i_bdd->child1 = i.id;
            id_map[i.id] = bdds.size;
            array_push_back(&bdds, inter);
            layout->inters[j++] = i;
        }
    }
    layout->inters.size = j;}

    // Create the new intermediates necessary.
    for (s64 i = 0; i < bdds.size; ++i) {
        if (bdds[i].child0 and bdds[id_map[bdds[i].child0]].level + 1 < bdds[i].level) {
            Bdd inter = {bdds[i].child0, bdds[i].child0, (u8)(bdds[i].level-1), Bdd::INTERMEDIATE, (u32)id_map.size};
            bdds[i].child0 = id_map.size;
            array_push_back(&id_map, (u32)bdds.size);
            array_push_back(&bdds, inter);
            array_push_back(&layout->inters, {bdds[i].id, inter.child0, inter.id});
            if (bdds[i].child1 == inter.child0) {
                bdds[i].child1 = bdds[i].child0;
                continue;
            }
        }
        if (bdds[i].child1 and bdds[id_map[bdds[i].child1]].level + 1 < bdds[i].level) {
            Bdd inter = {bdds[i].child1, bdds[i].child1, (u8)(bdds[i].level-1), Bdd::INTERMEDIATE, (u32)id_map.size};
            bdds[i].child1 = id_map.size;
            array_push_back(&id_map, (u32)bdds.size);
            array_push_back(&bdds, inter);
            array_push_back(&layout->inters, {bdds[i].id, inter.child0, inter.id});
        }
    }

    // The next step is initialising the positions for new nodes. This is done using very crude
    // heuristics, which basically just try to get the ordinal positions right. (So 'a left of b' or
    // 'b left of a', instead of 'a is 0.5 left of b'.)
    array_resize(&memory->pos, bdds.size);
    Array_t<Pos_id> pos = memory->pos;
    memset(pos.data, 0, pos.size * sizeof(pos[0]));

    // Initialise the nodes from the previous layout
    for (u32 i_it = 0; i_it < layout->bdd_pos.size; ++i_it) {
        Pos_id i = layout->bdd_pos[i_it];
        if (id_map[i.id] != (u32)-1) {
            pos[id_map[i.id]] = {i.x, i.y, i.id, Pos_id::FIXED | Pos_id::INITIALIZED};
        }
    }

    // Calculate the maximum x value of the previous nodes
    float x_max = 0.f;
    for (Pos_id i: pos) {
        if (not (i.flags & Pos_id::INITIALIZED)) continue;
        if (i.x > x_max) x_max = i.x;
    }

    // We want to ensure that all new nodes are initially to the very right of the old nodes in each
    // layer, even after the old nodes are inserted into the graph. This uses that a single node is
    // placed at most 1 to the right of a node already existing at that time.
    //  Bounding the total increase by bdds.size seems very mathematical and prone to rounding
    // errors, but keep in mind that I only want this to work for about ~1000 nodes and floats have
    // 24-bits of precision when representing integers. 1000 < 4000000
    for (u32 i = 0; i < bdds.size; ++i) {
        if (not (pos[i].flags & Pos_id::FIXED)) {
            pos[i].x = x_max + 1.f + (float)bdds.size;
            pos[i].y = bdds[i].level;
            pos[i].id = bdds[i].id;
        }
        if (pos[i].y != bdds[i].level) {
            pos[i].y = bdds[i].level;
            pos[i].flags = Pos_id::REPOSITION | Pos_id::INITIALIZED;
        }
    }

    // The order of the pos array swaps between id-based and position-based, depending on what is
    // more convenient for the current algorithm. Previously, it was id-based. Now, we want to do
    // the actual layout and are very interested in having the array reflect the nodes' position in
    // space. The precise ordering orders the nodes top-to-bottom (y decreasing) and then
    // left-to-right (x increasing).
    auto cmp1 = [](Pos_id a, Pos_id b) -> bool {
        if (a.y != b.y) return a.y > b.y;
        if ((a.flags ^ b.flags) & Pos_id::REPOSITION) return b.flags & Pos_id::REPOSITION;
        return a.x < b.x;
    };
    std::sort(pos.begin(), pos.end(), cmp1);

    // Note to myself: Maybe the following code would be much simpler if it just initialised the
    // x-values somewhat correctly, sorted the array, and then patched them up to prevent two nodes
    // being in the same position?

    // Take the node at j and insert it somewhere close to target_x
    auto insert_node = [&pos, &x_max](s64 j, float target_x) {
        // First, we find the node just to the right of target_x
        s64 k = j;
        while (0 < k and target_x < pos[k-1].x and pos[k-1].y == pos[j].y) --k;
        assert(k == 0 or pos[k].y == pos[j].y);

        // Now, we move the node from j into k, moving the others along.
        Pos_id pos_j = pos[j];
        for (s64 k_end = j; k_end >= k+1; --k_end) {
            pos[k_end] = pos[k_end - 1];
        }
        pos[k] = pos_j;

        // The precise insertion depends on whether there is a node to the left and/or to the right.
        bool exist_l = k > 0 and pos[k-1].y == pos[k].y;
        bool exist_r = k+1 < pos.size and pos[k+1].flags & Pos_id::INITIALIZED and pos[k+1].y == pos[k].y;

        if (exist_l and exist_r) {
            pos[k].x = pos[k-1].x * 0.5f + pos[k+1].x * 0.5f;
        } else if (exist_l) {
            pos[k].x = pos[k-1].x + 1.f;
            if (x_max < pos[k].x) x_max = pos[k].x;
        } else if (exist_r) {
            pos[k].x = pos[k+1].x - 1.f;
        } else {
            pos[k].x = target_x;
        }
        pos[k].flags |= Pos_id::INITIALIZED;
        return k;
    };
    
    // This code actually inserts the new nodes into the layout
    for (s64 i = 0; i < pos.size; ++i) {
        if (pos[i].flags & Pos_id::REPOSITION) {
            pos[i].flags &= ~Pos_id::REPOSITION;
            insert_node(i, pos[i].x);
            continue;
        }
        
        // If the node is not initialised here, that means that it has no parents. Sad. So, just put
        // it in somewhere to the right. (But still left of the uninitialised nodes!)
        if (not (pos[i].flags & Pos_id::INITIALIZED)) {
            x_max += 1.f;
            pos[i].x = x_max;
            pos[i].flags |= Pos_id::INITIALIZED;
        }
        Bdd i_bdd = bdds[id_map[pos[i].id]];

        // Test for F or T
        if (i_bdd.child0 == i_bdd.id) continue;

        // Find the two children, if they are not F
        s64 j0 = -(i_bdd.child0 == 0);
        s64 j1 = -(i_bdd.child1 == 0);
        for (s64 j = i+1; j < pos.size; ++j) {
            if (pos[j].id == i_bdd.child0) j0 = j;
            if (pos[j].id == i_bdd.child1) j1 = j;
            if (j0 and j1) break;
            assert(pos[j].y >= pos[i].y - 1);
        }
        assert(j0 and j1);
        assert(j0 == -1 or pos[j0].y == pos[i].y - 1);
        assert(j1 == -1 or pos[j1].y == pos[i].y - 1);

        // Insert child0 (unless it is already initialised)
        if (j0 != -1 and not (pos[j0].flags & Pos_id::INITIALIZED)) {
            s64 k = insert_node(j0, pos[i].x);
            j1 += k <= j1 and j1 < j0; // If the insertion of child0 would change j1, we have to patch it here.
        }
        // Insert child1 (unless it is already initialised). Note: Not sure whether the child0 !=
        // child1 check is actually necessary.
        if (j1 != -1 and i_bdd.child0 != i_bdd.child1 and not (pos[j1].flags & Pos_id::INITIALIZED)) {
            insert_node(j1, pos[i].x);
        }
    }

    // If a node moves just a tiny bit, we want to snap it back to its original position. So, save those.
    array_resize(&memory->pos_orig, pos.size);
    Array_t<float> pos_orig = memory->pos_orig;
    for (s64 i = 0; i < pos.size; ++i) {
        pos_orig[i] = pos[i].x;
    }

    // This maps ids -> positions. Note that the order of pos is currently position-based.
    array_resize(&memory->pos_map, id_map.size);
    Array_t<u32> pos_map = memory->pos_map;
    memset(pos_map.data, -1, pos_map.size * sizeof(u32));
    for (s64 i = 0; i < pos.size; ++i) {
        pos_map[pos[i].id] = i;
    }

    // This part swaps the order of two adjacent nodes if that would decrease the number of
    // edge-crossings, until no such swap is possible. Two adjacent fixed nodes are not swapped.
    while (true) {
        bool dirty = false;

        s64 prev_beg = 0; // First index of the previous layer
        s64 prev_end = 0; // First index of the current layer
        for (s64 i = 0; i+1 < pos.size; ++i) {
            if (pos[i].y != pos[i+1].y) {
                prev_beg = prev_end;
                prev_end = i+1;
                continue;
            }
            if (pos[i].flags & pos[i+1].flags & Pos_id::FIXED) continue;

            // diff stores the change in number of edge crossings, if we perform the swap. (So,
            // negative means we do the swap.)
            s64 diff = 0;

            // Count the number of parent edges that the swap would affect.
            for (s64 j0 = prev_beg; j0+1 < prev_end; ++j0) {
                // Node on bdd children: There are the following configurations for children of a node:
                //  - Two different bdds, child0 != child1 and child0 and child1
                //  - Only one child, (child0 == 0 or child1 == 0) and child0 != child1
                //  - No children, child0 == 0 and child1 == 0. This node has to be temporary or intermediate.
                //  - Twins, child0 == child1 and child0. This node has to be temporary or intermediate.
                //  - For all intermediate nodes, child0 == child1 holds.
                // Usually, it does not matter whether a node has twins or just one child. However,
                // here we actually need to differentiate between them, because a single node with
                // two edges should be counted differently than an intermediate node.


                // j0_i1_edge counts the number of edges from j0 to i+1. Analogously for the others.
                
                Bdd j0_bdd = bdds[id_map[pos[j0].id]];
                bool j0_f = not (j0_bdd.flags & Bdd::INTERMEDIATE);
                s64 j0_i0_edge = (j0_bdd.child0 == pos[i  ].id) + (j0_bdd.child1 == pos[i  ].id and j0_f);
                s64 j0_i1_edge = (j0_bdd.child0 == pos[i+1].id) + (j0_bdd.child1 == pos[i+1].id and j0_f);
                if (j0_i0_edge + j0_i1_edge == 0) continue;
                
                for (s64 j1 = j0+1; j1 < prev_end; ++j1) {
                    Bdd j1_bdd = bdds[id_map[pos[j1].id]];
                    bool j1_f = not (j1_bdd.flags & Bdd::INTERMEDIATE);
                    s64 j1_i0_edge = (j1_bdd.child0 == pos[i  ].id) + (j1_bdd.child1 == pos[i  ].id and j1_f);
                    s64 j1_i1_edge = (j1_bdd.child0 == pos[i+1].id) + (j1_bdd.child1 == pos[i+1].id and j1_f);
                    if (j1_i0_edge + j1_i1_edge == 0) continue;

                    // You might think that that multiplication is just an 'and'. But it really is a
                    // multiplication, as those variables count the number of edges, and the number
                    // of crossing is the product of those.
                    diff += j0_i0_edge*j1_i1_edge - j1_i0_edge*j0_i1_edge;
                }
            }

            Bdd i0_bdd = bdds[id_map[pos[i  ].id]];
            Bdd i1_bdd = bdds[id_map[pos[i+1].id]];

            // Count the number of children edges that the swap would affect.
            auto children_sign = [&pos, &pos_map](u32 c0, u32 c1) {
                if (not c0 or not c1) return 0;
                // c0 == c1 handled implicitly
                return (pos[pos_map[c0]].x < pos[pos_map[c1]].x) - (pos[pos_map[c0]].x > pos[pos_map[c1]].x);
            };
            diff += children_sign(i0_bdd.child0, i1_bdd.child0);
            diff += children_sign(i0_bdd.child0, i1_bdd.child1);
            diff += children_sign(i0_bdd.child1, i1_bdd.child0);
            diff += children_sign(i0_bdd.child1, i1_bdd.child1);

            if (diff < 0) {
                // Do the swap
                Pos_id tmp = pos[i];
                pos[i] = pos[i+1];
                pos[i+1] = tmp;
                pos[i+1].x = pos[i].x;
                pos[i].x = tmp.x;

                pos_map[pos[i  ].id] = i;
                pos_map[pos[i+1].id] = i+1;

                // Go back to the pair before the current one (if possible), so that the original
                // i+1 (now i) can be swapped further back directly. Optimised bubblesort ftw!
                i -= i > 0;
                --i;
                
                dirty = true;
            }
        }
        
        if (not dirty) break;
    }

    // Here is the part that does most of the 'making things look nice': The force-based node
    // layout. Also, it is the main bottleneck of the application, as I try to render about 100
    // frames with ~500 simulation steps each in the time it takes for the user to blink. (Currently
    // the algorithm takes ca. 200ms for graphs I consider at the very upper end of use cases.)

    // Generally speaking, the force simulation goes like this: Calculate some accelerations, add
    // them to the velocities, add those to the positions, and repeat until nothing moves
    // anymore. As I cannot guarantee convergence, there is a strict upper bound on the number of
    // iterations, and a quick exit if things are moving slow enough. The timestep is 1. I
    // experimented with using an adaptive timestep to quicken up convergence, but that did not work
    // out great.

    // General note on forces: Physical realism is not welcome here. Still, some principles are
    // useful to keep in mind:
    //  - Conservation of energy. If that principle holds, the simulation would not
    //    converge. However, forces that depend only on position will always preserve total
    //    energy. This is why we need friction.
    //  - Momentum (i.e. friction > 0). Not actually necessary, but I have found things to converge
    //    faster with it.
    //  - Conservation of impulse. Now, this is actually really important for things to look
    //    nice. If we don't have it, the graph will drift in one direction. 'No problem', you say,
    //    'just re-center it afterwards!' However, on what? Center of mass would be probably the
    //    best thing, but the mass changes in each animation frame. In other words, lots of
    //    effort. Just fix your forces and make Newton happy.
    //  - Everything is Continuous and Differentiable Infinitely Often. I do not now whether that is
    //    actually a physical law with a fancy name, but it is certainly important here. If the
    //    forces are not continuous, the algorithm will not necessarily converge but rather 'bounce'
    //    around the discontinuity. (Differentiable is not really needed.)
    
    constexpr float force_node     = 0.035f; // How much do the nodes push each other apart
    constexpr float force_edge     = 0.002f; // How much do the edges pull them together
    constexpr float force_inter    = 0.01f;  // How hard do the long edges try to straighten out
    constexpr float force_friction = 0.93f;  // How much friction is there
    constexpr float dist_min       = 0.61f;  // The smallest distance two nodes can have (see note on node separation below)
    constexpr float dist_pad       = 0.1f;   // Buffer zone for the process keeping nodes apart

    // Velocities for the nodes. They only move horizontally, so a single float is fine.
    array_resize(&memory->vel, pos.size);
    Array_t<float> vel = memory->vel;
    memset(vel.data, 0, vel.size * sizeof(vel[0]));

    // Buffer for the node-separation calculations (see note on node separation below)
    Array_dyn<float> vel_buf = memory->vel_buf;
    vel_buf.size = 0;
    defer { memory->vel_buf = vel_buf; };

    // Acceleration structure. Store as a bitset whether each node is an intermediate.
    array_resize(&memory->pos_inter_flag, (pos.size+63)/64);
    Array_t<u64> pos_inter_flag = memory->pos_inter_flag;
    for (s64 i = 0; i < pos.size; ++i) {
        bitset_set(&pos_inter_flag, i, (bool)(bdds[id_map[pos[i].id]].flags & Bdd::INTERMEDIATE));
    }

    // Acceleration structure. Store all the parent-child relationships we need to apply edge forces to.
    Array_dyn<Child_edge> pos_children = memory->pos_children;
    pos_children.size = 0;
    defer { memory->pos_children = pos_children; };

    for (u32 i = 0; i < pos.size; ++i) {
        Bdd i_bdd = bdds[id_map[pos[i].id]];
        if (i_bdd.child0) {
            array_push_back(&pos_children, {i, pos_map[i_bdd.child0]});
        }
        if (i_bdd.child1 and i_bdd.child0 != i_bdd.child1) {
            array_push_back(&pos_children, {i, pos_map[i_bdd.child1]});
        }
    }

    // Note on node distances: I generally want intermediate nodes to fit between normal
    // ones. Certainly they should not be as far apart. To realise this, I multiply distances by 2
    // for each intermediate node involved. (You could say that the algorithms uses a curved
    // space-time topology!) Effectively, this means that the forces pushing nodes apart and
    // separating them only keep them half as far apart as they would normal nodes. So the patterns
    // Node-Node and Node-Inter-Node have the nodes at the same distance, and Node-Inter-Inter-Node
    // is just at 1.25 fo that.
    
    // See note on node separation below
    auto dist_sticking_fac = [pos, pos_inter_flag](u32 i) mutable {
        assert(i+1 < pos.size);
        float d = pos[i+1].x - pos[i].x;
        // See note on node distances above.
        if (bitset_get(pos_inter_flag, i  )) d *= 2.f;
        if (bitset_get(pos_inter_flag, i+1)) d *= 2.f;
        // See note on node separation below.
        d = 1.f - (d - dist_min) / dist_pad;
        return std::max(std::min(d, 1.f), 0.f);
    };
    
    for (s64 iter = 0; iter < iter_max; ++iter) {
        // Friction. This is very important, as it gets energy out of the system. Without this,
        // everything would bounce in perpetuity.
        for (u32 i = 0; i < pos.size; ++i) {
            vel[i] *= force_friction;
        }

        // Apply the edge forces. Very simple, due to our acceleration structure.
        for (Child_edge i: pos_children) {
            // Note that the force is simply linear with horizontal distance. Realistic? No, but
            // does not have to be.
            float f = (pos[i.parent].x - pos[i.child].x) * force_edge;
            vel[i.parent] -= f;
            vel[i.child] += f;
        }

        // Apply the force making the long edges straight.
        for (Inter i: layout->inters) {
            u32 i0 = pos_map[i.parent];
            u32 i1 = pos_map[i.id];
            u32 i2 = pos_map[bdds[id_map[i.id]].child0];
            float f = (pos[i0].x + pos[i2].x) * 0.5 - pos[i1].x;
            vel[i1] += std::max(std::min(f, 0.1f), -0.1f) * force_inter;
            vel[i0] -= std::max(std::min(f, 0.1f), -0.1f) * force_inter * 0.5f;
            vel[i2] -= std::max(std::min(f, 0.1f), -0.1f) * force_inter * 0.5f;
        }

        // Note on node separation: Keeping the nodes apart is actually a somewhat interesting
        // problem. You probably think something like 'Just make the force inversely proportial to
        // distance, right?', but that has its own tradeoffs. If your force is infinite at some
        // positive distance dist_min, then it cannot handle two nodes being closer together than
        // dist_min. (At least, I encountered a lot of problems doing that.) So you would need a
        // pre-processing step ensuring that all nodes are sufficiently far apart, and while I think
        // that it would _likely_ work, its also a lot of effort.
        //  You may also think, 'Then just make the force pushing nodes apart larger!' But that
        // force only applies to neighbouring nodes, while the number of edges pulling on a single
        // node is not bounded. So, at some point it is going to get overwhelmed. Meanwhile, for
        // ordinary graphs the nodes are too far apart to even say hello to each other. :(
        //  So, I came up with the following solution. If two nodes are closer than the minimum
        // distance, they are 'glued' together, i.e. forces affect them both equally. In particular,
        // this means that no force can push them further together. The once exception is the force
        // pushing nodes apart, which still affects them as normal, and is thus uncontested.
        //
        //  In more precise terms, we want to do the following: If two nodes are closer than the
        // minimum distance, we ensure that they move into the same direction (preserving their
        // combined impulse). The node force is applied afterwards.
        //
        //  'But wait,' you might object. 'Doesn't that violate continuity?' Yeah, it does. This is
        // where things become a little bit ugly. Instead of saying 'two nodes are glued together'
        // we could also say 'for each node, the forces applied to that node are instead spread
        // evenly between all glued nodes'. So we introduce a 'glue factor'. (I want to stress that
        // I use this term much more naturally than the SAT-solver folks!)  If nodes are closer
        // together than dist_min, it is 1. If they are farther apart than dist_min+dist_buf, it is
        // 0. Between those it is a linear transition. Imagine the following nodes:
        //
        //     0 -- 1 -- 2 -- 3
        //       g1   g2   g3
        //
        // where g1, g2, g3 are the glue factors between nodes (0,1), (1,2), and (2,3),
        // respectively. Now, consider things from the point of view of node i: Instead of the
        // forces originally applied to i being spread evenly, we assign each node a weight
        // determining how much force it gets:
        //
        //     Node | Weight 0 | Weight 1 | Weight 2 | Weight 3
        //        0 |        1 |       g1 |    g1*g2 | g1*g2*g3
        //        1 |       g1 |        1 |       g2 |    g2*g3
        //        2 |    g1*g2 |       g2 |        1 |       g3
        //        3 | g1*g2*g3 |    g2*g3 |       g3 |        1
        //
        // Consider the row for node 1. It meas, that when computing how to distribute the force
        // applied to node 1, node 0 has a weight of g1, node 1 has a weight of 1, and so on. That
        // intuition behind that is that for node 1, the force does not have to travel through any
        // glue links, so it has the full weight of 1. For node 0, it has to travel through the
        // (0,1) link. If the nodes are close enough together, this weight is also 1, but if they
        // are just within the buffer zone, you get a factor 0 < g1 < 1. For node 3, the force has
        // to travel through both the (1,2) and the (2,3) links, so we multiply those together.
        //  This does everything we want: The matrix is symmetric, so the same fraction of force
        // flows in either direction of each pair. If nodes are closer together than dist_min, they
        // are glued together perfectly. If they are farther apart than dist_min+dist_buf, no force
        // is exchanged. And everything is nicely continuous inbetween.

        // TODO: As I was to lazy to have a separate array storing accelerations, the momentum from
        // the previous step is also distributed, including the one generated by the separating
        // force. This algorithm would likely work better if that were not the case. Maybe even
        // significant performance wins.
        
        for (s64 i = 0; i+1 < pos.size; ++i) {
            // Note the the above is an n**2 algorithm, if implemented naively. Instead, we can
            // split all nodes into smaller groups whenever a link has glue factor 0.

            // Find the last member in group
            s64 i1 = i+1;
            while (i1 < pos.size and pos[i].y == pos[i1].y) {
                if (pos[i1].x - pos[i1-1].x > dist_min + dist_pad) break;
                if (dist_sticking_fac(i1-1) == 0.f) break;
                ++i1;
            }
            // If there is only one node, nothing needs to be done
            if (i+1 == i1) continue;

            vel_buf.size = 0;
            array_append_zero(&vel_buf, i1 - i);

            // Distribute the forces into vel_buf
            for (s64 j = i; j < i1; ++j) {
                float fac_sum = 1.f;
                float fac = 1.f;
                for (s64 j1 = j-1; j1 >= i; --j1) {
                    fac *= dist_sticking_fac(j1);
                    fac_sum += fac;
                }
                fac = 1.f;
                for (s64 j1 = j+1; j1 < i1; ++j1) {
                    fac *= dist_sticking_fac(j1-1);
                    fac_sum += fac;
                }
                vel_buf[j-i] += vel[j] / fac_sum;
                fac = 1.f;
                for (s64 j1 = j-1; j1 >= i; --j1) {
                    fac *= dist_sticking_fac(j1);
                    vel_buf[j1-i] += vel[j] * fac / fac_sum;
                }
                fac = 1.f;
                for (s64 j1 = j+1; j1 < i1; ++j1) {
                    fac *= dist_sticking_fac(j1-1);
                    vel_buf[j1-i] += vel[j] * fac / fac_sum;
                }
            }

            // Copy them back into vel
            for (s64 j = i; j < i1; ++j) {
                vel[j] = vel_buf[j-i];
            }

            // Skip the rest of the group
            i = i1-1;
        }

        // Apply the force keeping nodes apart.
        for (s64 i = 0; i + 1 < pos.size; ++i) {
            if (pos[i].y != pos[i+1].y) continue;
            
            // See note an node distances above.
            float d = pos[i+1].x - pos[i].x;
            if (bitset_get(pos_inter_flag, i  )) d *= 2.f;
            if (bitset_get(pos_inter_flag, i+1)) d *= 2.f;
            
            float f = -std::log2(d);
            if (d > 1.f) f *= 0.01f; // This is still continuous, although certainly not elegant
            f *= force_node;
            vel[i  ] -= f;
            vel[i+1] += f;
        }

        // Ensure that the order of nodes is preserved
        for (u32 i = 0; i+1 < pos.size; ++i) {
            if (pos[i].y != pos[i+1].y) continue;
            if (pos[i].x + vel[i] >= pos[i+1].x + vel[i+1]) {
                float f = (pos[i+1].x - pos[i].x) / (vel[i] - vel[i+1]) * 0.95f;
                vel[i] *= f;
                vel[i+1] *= f;
            }
        }

        // Update the positions
        for (u32 i = 0; i < pos.size; ++i) {
            pos[i].x += vel[i];
        }
        
        // Maximum velocity. Use for early termination.
        float vel_max = 0.f;
        for (u32 i = 0; i < pos.size; ++i) {
            if (vel_max < std::abs(vel[i])) vel_max = std::abs(vel[i]);
        }
        if (vel_max < 1e-6f) break;
    }

    // Snap back the nodes that did not move a lot compared to their original position, to prevent
    // sub-pixel weirdness and aliasing issues. May not be necessary anymore, with all the fancy
    // antialiasing that is going on.
    for (u32 i = 0; i < pos.size; ++i) {
        if (std::abs(pos[i].x - pos_orig[i]) < 0.01) {
            pos[i].x = pos_orig[i];
        }
    }

    // As we are now done with the positioning, change the order to id-based again
    auto cmp2 = [&id_map](Pos_id a, Pos_id b) {
        return id_map[a.id] < id_map[b.id];
    };
    std::sort(pos.begin(), pos.end(), cmp2);

    // Finally, we write our results back into layout
    layout->bdd_pos.size = 0;
    array_append(&layout->bdd_pos, pos);
    layout->id_max = id_map.size;

    layout->edges[0].type = Edge::INVALID;
    layout->edges.size = 1; // Keep the extra element
    layout->edge_data.size = 0;

    // Write back a single edge, by chasing the chain of intermediates
    auto do_edge = [layout, &pos, &bdds, &id_map](u32 i, bool child) {
        Edge* e = &layout->edges[layout->edges.size-1];
        e->type = child ? Edge::CHILD1 : Edge::CHILD0;
        e->from = i;
        
        array_push_back(&layout->edge_data, Pos {pos[i].x, pos[i].y});
        u32 j = i;
        while (true) {
            j = id_map[child ? bdds[j].child1 : bdds[j].child0];
            array_push_back(&layout->edge_data, Pos {pos[j].x, pos[j].y});
            if (not (bdds[j].flags & Bdd::INTERMEDIATE)) break;
        }
        e->to = j;
        array_push_back(&layout->edges, {(u64)layout->edge_data.size, Edge::INVALID, 0, 0});
    };

    // Write all the edges
    for (s64 i = 0; i < bdds.size; ++i) {
        if (bdds[i].child0 == bdds[i].id) continue;
        if (bdds[i].flags & Bdd::INTERMEDIATE) continue;

        if (bdds[i].child0) {
            do_edge(i, 0);
        }
        if (bdds[i].child1) {
            do_edge(i, 1);
        }
    }
}

// Takes the straight edges generated by layout_graph and makes quadratic splines out of them. There
// is no need to adjust storage, as all we need to store is a list of points, regardless.
void layout_splinify(Bdd_layout* layout, Layout_memory* memory) {
    struct Layout_splinify_param {
        float slope_min = 0.6f; // At what slope does the desloper kick in
        float slope_fac = 0.4f; // How much of the original slope is retained by the desloper
        float perturbation = 0.08f; // How much two parallel edges should be adjusted
    };
    Layout_splinify_param param;

    // The new edges are first written into edge_data_new and then copied into layout->edge_data
    Array_dyn<Pos> edge_data_new = memory->edge_data_new;
    edge_data_new.size = 0;
    defer { memory->edge_data_new = edge_data_new; };

    // Temporary scratchpad for the current node
    Array_dyn<Pos> i_data = memory->i_data;
    defer { memory->i_data = i_data; };
    i_data.size = 0;
    
    for (s64 i = 0; i+1 < layout->edges.size; ++i) {
        Edge* e = &layout->edges[i];
        Array_t<Pos> i_data_orig = array_subarray(layout->edge_data, e->offset, layout->edges[i+1].offset);
        e->offset = edge_data_new.size;

        // Detect whether the current edge is parallel to its sibling
        bool same = e->type == Edge::CHILD0
            ? i+2 < layout->edges.size and layout->edges[i+1].from == e->from and layout->edges[i+1].to == e->to
            : i   > 0                  and layout->edges[i-1].from == e->from and layout->edges[i-1].to == e->to;

        i_data.size = 0;
        array_append(&i_data, i_data_orig);

        // Note on desloping: Edges that move very far in the horizontal direction are not nice for
        // a variety of reasons. Mostly, they pass through neighbouring nodes in the process. If we
        // detect an edge to have a small enough slope, we instead replace it by three edges, so
        // that the first and the last have more slope. While this transformation is done on
        // straight edges, it only really makes sense if we later generate splines out of them.
        
        // This is the desloper. It only kicks in for direct edges.
        if (i_data.size == 2) {
            Pos p0 = i_data[0];
            Pos p1 = i_data[1];
            float slope = (p1.y - p0.y) / (p1.x - p0.x);
            if (std::abs(slope) < param.slope_min) {
                float s = slope * (1.f - param.slope_fac) + std::copysign(param.slope_min, slope) * param.slope_fac;
                Pos p2 = {p0.x + (p1.y - p0.y) * 0.33f / s, p0.y * 0.67f + p1.y * 0.33f};
                Pos p3 = {p1.x - (p1.y - p0.y) * 0.33f / s, p0.y * 0.33f + p1.y * 0.67f};
                array_insert(&i_data, 1, p2);
                array_insert(&i_data, 2, p3);
            }
        }

        // Here we offset edges a bit if they are part of a parallel pair
        if (same) {
            if (i_data.size == 2) {
                array_insert(&i_data, 1, {
                    (i_data[0].x + i_data[1].x) * 0.5f, (i_data[0].y + i_data[1].y) * 0.5f
                });
            }
            for (s64 j = 1; j+1 < i_data.size; ++j) {
                float ux = i_data[j-1].y - i_data[j+1].y;
                float uy = i_data[j+1].x - i_data[j-1].x;
                float len = std::sqrt(ux*ux + uy*uy) / param.perturbation;
                if (e->type == Edge::CHILD0) len *= -1.f;
                ux /= len; uy /= len;
                i_data[j].x += ux;
                i_data[j].y += uy;
            }
        }

        // For debugging purposes, you define this and generate straight lines instead of curvy splines. (This rhymes!)
#ifndef DBG_DRAW_STRAIGHT_LINES

        // The following code takes the straight edges and inserts control points for the quadrativ
        // bezier curves.
        
        // The process of converting the straight line segments into quadratic splines is a bit
        // tricky. There are three main conditions this tries to fulfill:
        //  - The lines have to be continuous
        //  - The lines have to be differentiable
        //  - The tangent at a point is the parallel to the line between the previous and the next
        //    point
        // Sometimes it is necessary to insert new points in the middle, as we only have quadratic
        // splines and not cubic ones.
        //  The following code makes some assumptions that lines are moving in y-direction and is
        // probably not going to work great in the general case.
        s64 l = i_data.size;
        if (l == 2) {
            // Two points make a straight line. This case is not handled by the code below.
            Pos c0 = _pos_mix(i_data[0], i_data[1], 0.5f);
            array_append(&edge_data_new, {i_data[0], c0, i_data[1]});
        } else {
            // For the beginning and end, we just need to insert a control point on the right
            // line. That leaves one dimension free, so we choose the control point to be precisely
            // in the middle of i_data[0] and i_data[1] in terms of y-axis.
            {float f = (i_data[0].y - i_data[1].y) / (i_data[0].y - i_data[2].y) * 0.5f;
            Pos c0 = {
                i_data[1].x + f * (i_data[0].x - i_data[2].x),
                0.5f*i_data[0].y + 0.5f*i_data[1].y
            };
            array_append(&edge_data_new, {i_data[0], c0, i_data[1]});}

            for (s64 j = 1; j+2 < l; ++j) {
                // Now, this is a little bit more complicated. We want:
                //  - Tangent in m1 parallel to m0-m2
                //  - Tangent in m2 parallel to m1-m3
                // This determines one axis each, for the control point after m1 (called c0) and the
                // one before m2 (called c2). (Which are different points, else we cannot solve this
                // in general.) The second dimension is once again chosen in terms of their
                // y-coordinate: c0 is at 0.75*m1 + 0.25*m2, c2 at 0.25*m1 + 0.75*m2. To be
                // differentiable, the point we insert between those two control points needs to lie
                // on the line between c0 and c2, the middle works just fine.
                
                Pos m0 = i_data[j-1];
                Pos m1 = i_data[j  ];
                Pos m2 = i_data[j+1];
                Pos m3 = i_data[j+2];
                float f0 = 0.25f * (m2.y - m1.y) / (m2.y - m0.y);
                float f1 = 0.25f * (m1.y - m2.y) / (m1.y - m3.y);
                Pos c0 = {m1.x + f0 * (m2.x - m0.x), 0.75f*m1.y + 0.25f*m2.y};
                Pos c2 = {m2.x + f1 * (m1.x - m3.x), 0.25f*m1.y + 0.75f*m2.y};
                Pos c1 = _pos_mix(c0, c2, 0.5f);
                array_append(&edge_data_new, {c0, c1, c2, m2});
            }

            // Same as for the beginning.
            {float f = (i_data[l-1].y - i_data[l-2].y) / (i_data[l-1].y - i_data[l-3].y) * 0.5f;
            Pos c1 = {
                i_data[l-2].x + f * (i_data[l-1].x - i_data[l-3].x),
                0.5f*i_data[l-1].y + 0.5f*i_data[l-2].y
            };
            array_append(&edge_data_new, {c1, i_data[l-1]});}
        }
        
#else
        // Generate the control points exactly in the middle
        s64 l = i_data.size;
        array_push_back(&edge_data_new, i_data[0]);
        for (s64 j = 1; j < l; ++j) {
            array_append(&edge_data_new, {
                _pos_mix(i_data[j-1], i_data[j], 0.5f), i_data[j]
            });
        }
#endif
    }
    layout->edges[layout->edges.size - 1].offset = edge_data_new.size;

    layout->edge_data.size = 0;
    array_append(&layout->edge_data, edge_data_new);
}

struct Draw_param {
    // All units here are in world coordinates
    
    float node_radius = 0.3f; // x-radius of the nodes
    float squish_fac = 0.7f; // Ratio between the x-radius and the y-radius (ry = rx * squish_fac)
    float edge_size = 0.008f; // Thickness of an edge
    float dash_length = 0.1f; // Length of the periodic segment of a dashed line (so between the start of one gap and the start of the next)
    float arrow_size = 0.1f; // Size of an arrow from the tip to the base
    float edge_point_merge = 0.05f; // How far apart to points on an edge can be in terms of y-axis and still be considered the same for linear interpolation
    float font_frac = 0.9f; // Ratio between font size and node y radius (font_size = ry * font_frac)
};

// Data needed to draw a bdd
struct Bdd_attr {
    float x = 0.f, y = 0.f; // Position of the centre in world space
    float rx = 0.f, ry = 0.f; // Radius in x and y direction
    float font_x = 0.f, font_y = 0.f; // Position of the font centre. Not the same as x and y, but at most a pixel off.
    float font_x2 = 0.f, font_y2 = 0.f; // Same, but slightly different offset of the second line of text
    float border = 0.05f;
    u8 stroke[4] = {}; // Color to draw the border, in RGBA
    u8 fill[4]   = {}; // Color to fill the area, in RGBA
    float mark_child0 = 0.f; // How much the child0 edge is marked
    float mark_child1 = 0.f; // How much the child1 edge is marked
    u32 name = 0; // Which label to draw.
    float name_alpha = 1.f;
};

// Keeps the necessary data to manage Opengl rendering
struct Opengl_context {
    // Indices for all the vertex attributes
    enum Attributes: GLuint {
        BDD_POS = 0, BDD_X, BDD_R, BDD_STROKE, BDD_FILL, BDD_BORDER, BDD_ATTR_COUNT,
        EDGE_POS = 0, EDGE_P0, EDGE_P1, EDGE_P2, EDGE_A, EDGE_DASH, EDGE_STROKE, EDGE_ATTR_COUNT,
        ARROW_POS = 0, ARROW_H, ARROW_FILL, ARROW_ATTR_COUNT,
        TEXT_POS = 0, TEXT_TPOS, TEXT_FILL, TEXT_ATTR_COUNT,
        RECT_POS = 0, RECT_FILL, RECT_ATTR_COUNT,
        UI_POS = 0, UI_FILL, UI_ATTR_COUNT,
    };
    // Names for all uniforms
    enum Uniforms: int {
        BDD_ORIGIN, BDD_SCALE, BDD_SCALE_P,
        EDGE_ORIGIN, EDGE_SCALE, EDGE_SCALE_P,
        ARROW_ORIGIN, ARROW_SCALE,
        TEXT_ORIGIN, TEXT_SCALE, TEXT_SAMPLER,
        RECT_ORIGIN, RECT_SCALE,
        UI_ORIGIN, UI_SCALE,
        UNIFORMS_COUNT
    };
    
    double width, height; // Dimensions of the canvas in pixels
    float origin_x, origin_y; // Position of the bottom-left corner of the screen in world-coordinates
    float scale; // Ratio of world-coordinates and pixels (world * scale = pixels)
    float layout_max_x, layout_max_y; // Position of the top-right corner of the screen in world-coordinates
    s64 layout_max_points; // Maximum number of points needed to be stored for edge data

    s64 canvas_x, canvas_y; // The offset of the canvas in pixels. Used by native UI, 0 for emscripten
    s64 screen_w, screen_h; // The dimensions of the screen in pixels. Used by native UI, for emscripten same as canvas
    
    Draw_param draw_param;
    float font_size_max; // Font size of a fully-sized node (i.e. rx == draw_param.node_radius)
    float font_small_frac = 0.68f; // The size of the small font is this fraction the size of the big one
    float font_linoff; // Difference between the ascent of the font and the height of an E. Only used for emscripten.
    float font_ascent; // The ascent of the font
    s64 font_regenerate; // Frames until font regenerates, after the user resizes the window

    bool not_completely_empty; // This is set if some graph is currently shown

    // Lots of GL boilerplate incoming...

    // Ids of the shader programs
    GLuint program_bdd;
    GLuint program_edge;
    GLuint program_arrow;
    GLuint program_text;
    GLuint program_rect;
    GLuint program_ui;
    // Ids of the uniforms
    Array_t<GLint> uniforms;

    GLuint text_tex; // Id of the text glyph texture
    Array_t<Text_box> text_pos; // Array of positions mapping each character to a rectangle (x0, y0, x1, y1) of the text glyph texture

    // Buffers for the vertex attribute arrays
    Array_dyn<float> buf_bdd_pos;
    Array_dyn<float> buf_bdd_x;
    Array_dyn<float> buf_bdd_r;
    Array_dyn<u8>    buf_bdd_stroke;
    Array_dyn<u8>    buf_bdd_fill;
    Array_dyn<float> buf_bdd_border;

    Array_dyn<float> buf_edge_pos;
    Array_dyn<float> buf_edge_p0;
    Array_dyn<float> buf_edge_p1;
    Array_dyn<float> buf_edge_p2;
    Array_dyn<float> buf_edge_a;
    Array_dyn<float> buf_edge_dash;
    Array_dyn<u8>    buf_edge_stroke;

    Array_dyn<float> buf_arrow_pos;
    Array_dyn<float> buf_arrow_h;
    Array_dyn<u8>    buf_arrow_fill;

    Array_dyn<float> buf_text_pos;
    Array_dyn<float> buf_text_tpos;
    Array_dyn<u8>    buf_text_fill;

    Array_dyn<float> buf_rect_pos;
    Array_dyn<u8>    buf_rect_fill;

    // Names of the buffers for the vertex attributes of each shader
    Array_t<GLuint> buffers_bdd;
    Array_t<GLuint> buffers_edge;
    Array_t<GLuint> buffers_arrow;
    Array_t<GLuint> buffers_text;
    Array_t<GLuint> buffers_rect;
    Array_t<GLuint> buffers_ui;

    Array_t<u8> buf_render; // Memory used during rendering
    Array_dyn<Bdd_attr> buf_attr_cur; // Current attributes used for the bdds. Mostly stored as convenience, the data is also stored in the vertex attribute buffers in some form.
};

// Load shader from string, print an error and abort if there is an error
GLuint opengl_shader_load(GLenum type, Array_t<u8> source, char* name) {
   GLuint shader = glCreateShader(type);

   if (not shader) {
       ui_error_report("Error: Could not create shader of type %d", type);
       abort();
   }

   GLint source_size = source.size - 1;
   glShaderSource(shader, 1, (GLchar**)&source.data, &source_size);
   glCompileShader(shader);
   
   GLint compiled;
   glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
   if (!compiled) {
      GLint info_size = 0;
      glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &info_size);

      char* info = (char*)malloc(info_size);
      glGetShaderInfoLog(shader, info_size, NULL, info);
      printf("Error while compiling shader %s:\n\n", name);
      puts(info);
      abort();
   }

   return shader;
}

// Link a shader program, print an error and abort if there is an error
void opengl_program_link(GLuint program, char* program_name) {
    glLinkProgram(program);
    GLint linked;
    glGetProgramiv(program, GL_LINK_STATUS, &linked);
    if (!linked) {
        GLint info_size = 0;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &info_size);
      
        char* info = (char*)malloc(info_size);
        glGetProgramInfoLog(program, info_size, NULL, info);
        printf("Error while linking program %s:\n\n", program_name);
        puts(info);
        abort();
    }
}

// (Re-)initialise the text glyph texture.
void opengl_text_prepare(Opengl_context* context) {
    if (context->text_tex != 0) {
        glDeleteTextures(1, &context->text_tex);
    }
    glGenTextures(1, &context->text_tex);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, context->text_tex);

    if (context->text_pos.size == 0) {
        context->text_pos = array_create<Text_box>(opengl_bddlabel_index_size);
    }

    int font_size = (int)std::round(
        context->draw_param.node_radius * context->draw_param.squish_fac * context->draw_param.font_frac * context->scale
    );

    float linoff, ascent;
    platform_text_prepare(font_size, context->font_small_frac, &context->text_pos, &linoff, &ascent);

    context->font_size_max = (float)font_size / context->scale;
    context->font_linoff   = (float)linoff    / context->scale;
    context->font_ascent   = (float)ascent    / context->scale;

    glGenerateMipmap(GL_TEXTURE_2D);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
}

// Initialised the OpenGL context
void opengl_init(Opengl_context* context) {
    // The shaders. Generally speaking, the vertex shader just maps into device coordinates while
    // passing on the relevant attributes, and the fragment shader then does all the calculations.

    // Note that the scale passed as attribute is not context->scale (ratio of pixels and
    // world->coordinates) but rather the factor between world-coordinates and device-coordinates.

#ifdef OBST_PLATFORM_EMSCRIPTEN
#define OBST_SHADER_F_PREAMBLE "precision mediump float;\n"
#define OBST_SHADER_TEXT_CHANNEL "a"
#else
#define OBST_SHADER_F_PREAMBLE "\n"
#define OBST_SHADER_TEXT_CHANNEL "r"
#endif
    
    GLbyte shader_v_bdd[] =  
        "attribute vec2 pos;\n"
        "attribute vec2 x;\n"
        "attribute vec2 r;\n"
        "attribute vec4 stroke;\n"
        "attribute float border;\n"
        "attribute vec4 fill;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "\n"
        "varying vec2 v_pos;\n"
        "varying vec2 v_x;\n"
        "varying vec2 v_r;\n"
        "varying vec4 v_stroke;\n"
        "varying float v_border;\n"
        "varying vec4 v_fill;\n"
        "\n"
        "void main() {\n"
        "    gl_Position = vec4((pos - origin)*scale, vec2(0.6 - stroke.a*0.1, 1));\n"
        "    v_pos = pos;\n"
        "    v_x = x;\n"
        "    v_r = r;\n"
        "    v_stroke = stroke;\n"
        "    v_border = border;\n"
        "    v_fill = fill;\n"
        "}\n";
    
    GLbyte shader_f_bdd[] =
        OBST_SHADER_F_PREAMBLE
        "varying vec2 v_pos;\n"
        "varying vec2 v_x;\n"
        "varying vec2 v_r;\n"
        "varying vec4 v_stroke;\n"
        "varying float v_border;\n"
        "varying vec4 v_fill;\n"
        "uniform float scale_p;\n"
        "\n"
        "void main() {\n"
        "    // We map into the space where the ellipsis becomes a circle by dividing by v_r (note v_r is a\n"
        "    // vec2). There, the length of a vector just indicates the factor needed to map it onto the\n"
        "    // ellipsis' border. So v_pos + (v_pos - v_x)*f would lie exactly on the ellipsis' border. Hence\n"
        "    // d is just the distance to the border in world-space on the line through v_x.\n"
        "    float f = length((v_pos - v_x) / v_r);\n"
        "    float d = (f-1.)*length(v_pos-v_x);\n"
        "    float e = 0.5*f*v_border*v_r.x;\n"
        "    // t1 is the antialias factor for the outer edge, t2 for the inner\n"
        "    float t1 = clamp((d - e)*scale_p+0.5, 0.0, 1.0);\n"
        "    float t2 = clamp((d + e)*scale_p+0.5, 0.0, 1.0);\n"
        "    vec4 col = mix(v_fill, v_stroke, t2);\n"
        "    col.a *= 1.0 - t1;\n"
        "    if (t1 >= 1.0) {\n"
        "        discard;\n"
        "    } else {\n"
        "        gl_FragColor = col;\n"
        "    }\n"
        "}\n";

    GLbyte shader_v_edge[] =
        "attribute vec2 pos;\n"
        "attribute vec2 p0;\n"
        "attribute vec2 p1;\n"
        "attribute vec2 p2;\n"
        "attribute vec2 a;\n"
        "attribute vec3 dash;\n"
        "attribute vec4 stroke;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "\n"
        "varying vec2 v_pos;\n"
        "varying vec2 v_p1;\n"
        "varying vec2 v_p2;\n"
        "varying vec2 v_a;\n"
        "varying vec3 v_dash;\n"
        "varying vec4 v_stroke;\n"
        "varying vec3 v_v;\n"
        "varying vec2 v_dg;\n"
        "\n"
        "void main() {\n"
        "    gl_Position = vec4((pos - origin)*scale, vec2(0.7 - stroke.a*0.1, 1));\n"
        "    v_pos = pos-p0;\n"
        "    v_p1 = p1-p0;\n"
        "    v_p2 = p2-p0;\n"
        "    v_a  = a;\n"
        "    v_dash = dash;\n"
        "    v_stroke = stroke;\n"
        "    \n"
        "    // For comments on the awesome math that happens here, please see the note on spline \n"
        "    // rendering below. (In the file obst.cpp, in case you are reading this in your browser.)\n"
        "    mat2 A = mat2(0.5*v_p2.y, -v_p1.y, -0.5*v_p2.x, v_p1.x) / (v_p1.x*v_p2.y-v_p1.y*v_p2.x);\n"
        "    mat2 At = mat2(A[0][0], A[1][0], A[0][1], A[1][1]);\n"
        "    vec2 Apos = A*v_pos;\n"
        "    v_v.y = Apos.y;\n"
        "    v_v.z = Apos.x + Apos.y;\n"
        "    v_dg = At*vec2(-2.0*v_v.z, -2.0*v_v.z+1.0);\n"
        "    v_v.x = dot(At[0]+At[1], v_dg);\n"
        "}\n";

    GLbyte shader_f_edge[] =
        OBST_SHADER_F_PREAMBLE
        "varying vec2 v_pos;\n"
        "varying vec2 v_p1;\n"
        "varying vec2 v_p2;\n"
        "varying vec2 v_a;\n"
        "varying vec3 v_dash;\n"
        "varying vec4 v_stroke;\n"
        "varying vec3 v_v;\n"
        "varying vec2 v_dg;\n"
        "uniform float scale_p;\n"
        "\n"
        "void main() {\n"
        "    // For comments on the awesome math that happens here, please see the note on spline \n"
        "    // rendering below. (In the file obst.cpp, in case you are reading this in your browser.)\n"
        "    float t = clamp(v_v.z-(v_v.y-v_v.z*v_v.z)*v_v.x/dot(v_dg, v_dg), 0.0, 1.0);\n"
        "    float d = distance(mix(v_p1*t,mix(v_p1,v_p2,t),t), v_pos);\n"
        "    float d2 = clamp((d - v_dash.x) * scale_p + 0.5, 0.0, 1.0);\n"
        "    float t2 = (((1.0-v_a.x-v_a.y)*t+v_a.y)*t+v_a.x)*t;\n"
        "    float d4 = abs(2.0*fract((t2 + v_dash.y) / v_dash.z + 0.25)-1.0)-0.5;\n"
        "    float d3 = clamp(d4 * v_dash.z * scale_p*0.5 + 0.5, 0.0, 1.0);\n"
        "    vec4 col = v_stroke;\n"
        "    col.a *= 1.0 - d2;\n"
        "    col.a *= 1.0 - d3;\n"
        "    \n"
        "    if (d3 >= 1.0) {\n"
        "        discard;\n"
        "    } else if (d2 >= 1.0) {\n"
        "        discard;\n"
        "    } else {\n"
        "        gl_FragColor = col;\n"
        "    }\n"
        "    // For debugging: This shows the control points. Comment the discards above out. \n"
        "    //if (length(v_pos) < 0.05) { gl_FragColor = vec4(1,0,0,1); };\n"
        "    //if (distance(v_p1, v_pos) < 0.05) { gl_FragColor = vec4(0,1,0,1); };\n"
        "    //if (distance(v_p2, v_pos) < 0.05) { gl_FragColor = vec4(0,0,1,1); };\n"
        "}\n";
    
    GLbyte shader_v_arrow[] =
        "attribute vec2 pos;\n"
        "attribute float h;\n"
        "attribute vec4 fill;\n"
        "varying vec4 v_fill;\n"
        "varying float v_h;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "void main() {\n"
        "    gl_Position = vec4((pos - origin)*scale, vec2(0.7 - fill.a*0.1, 1));\n"
        "    v_fill = fill;\n"
        "    v_h = h;\n"
        "}\n";

    GLbyte shader_f_arrow[] =
        OBST_SHADER_F_PREAMBLE
        "varying vec4 v_fill;\n"
        "varying float v_h;\n"
        "void main() {\n"
        "    vec4 col = v_fill;\n"
        "    col.a *= clamp(0.5-v_h , 0.0, 1.0);\n"
        "    gl_FragColor = col;\n"
        "}\n";
    
    GLbyte shader_v_text[] =
        "attribute vec2 pos;\n"
        "attribute vec2 tpos;\n"
        "attribute vec4 fill;\n"
        "varying vec2 v_tpos;\n"
        "varying vec4 v_fill;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "void main() {\n"
        "    gl_Position = vec4((pos - origin)*scale, 0.4, 1);\n"
        "    v_tpos = tpos;\n"
        "    v_fill = fill;\n"
        "}\n";

    GLbyte shader_f_text[] =
        OBST_SHADER_F_PREAMBLE
        "varying vec2 v_tpos;\n"
        "varying vec4 v_fill;\n"
        "uniform sampler2D sampler;\n"
        "void main() {\n"
        "    vec4 col = v_fill;\n"
        "    col.a *= texture2D(sampler, v_tpos)." OBST_SHADER_TEXT_CHANNEL ";\n"
        "    gl_FragColor = col;\n"
        "}\n";

    GLbyte shader_v_rect[] =
        "attribute vec3 pos;\n"
        "attribute vec4 fill;\n"
        "varying vec4 v_fill;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "void main() {\n"
        "    gl_Position = vec4((pos.xy - origin)*scale, vec2(pos.z, 1));\n"
        "    v_fill = fill;\n"
        "}\n";

    GLbyte shader_f_rect[] =
        OBST_SHADER_F_PREAMBLE
        "varying vec4 v_fill;\n"
        "void main() {\n"
        "    gl_FragColor = v_fill;\n"
        "}\n";
    

#undef OBST_SHADER_F_PREAMBLE

    // Yeah, sorry about the macros, but I do not think this code would be more readable without them.

#define OBST_LOAD_SHADER(x, y) \
    GLuint x##_id = opengl_shader_load(y, {(u8*)x, sizeof(x)}, (char*)#x); \
    glAttachShader(program, x##_id)

#define OBST_PROGRAM_INIT(x) \
    program = glCreateProgram(); \
    context->program_##x = program; \
    assert(program); \
    OBST_LOAD_SHADER(shader_v_##x, GL_VERTEX_SHADER); \
    OBST_LOAD_SHADER(shader_f_##x, GL_FRAGMENT_SHADER)

#define OBST_PROGRAM_LINK(x) \
    opengl_program_link(program, (char*)#x)

#define OBST_ATTRIB(x, y) \
    glBindAttribLocation(program, context->x, #y);
    
#define OBST_UNIFORM(x, y) \
    context->uniforms[context->x] = glGetUniformLocation(program, #y); \
    assert(context->uniforms[context->x] != -1)

#define OBST_GEN_BUFFERS(x, y) \
    context->buffers_##x = array_create<GLuint>(context->y##_ATTR_COUNT); \
    glGenBuffers(context->y##_ATTR_COUNT, context->buffers_##x.data)
    
    GLuint program;
    context->uniforms = array_create<GLint>(Opengl_context::UNIFORMS_COUNT);
    
    OBST_PROGRAM_INIT(bdd);
    OBST_ATTRIB(BDD_POS, pos);
    OBST_ATTRIB(BDD_X, x);
    OBST_ATTRIB(BDD_R, r);
    OBST_ATTRIB(BDD_STROKE, stroke);
    OBST_ATTRIB(BDD_BORDER, border);
    OBST_ATTRIB(BDD_FILL, fill);
    OBST_PROGRAM_LINK(bdd);
    OBST_UNIFORM(BDD_ORIGIN, origin);
    OBST_UNIFORM(BDD_SCALE, scale);
    OBST_UNIFORM(BDD_SCALE_P, scale_p);

    OBST_PROGRAM_INIT(edge);
    OBST_ATTRIB(EDGE_POS, pos);
    OBST_ATTRIB(EDGE_P0, p0);
    OBST_ATTRIB(EDGE_P1, p1);
    OBST_ATTRIB(EDGE_P2, p2);
    OBST_ATTRIB(EDGE_A, a);
    OBST_ATTRIB(EDGE_DASH, dash);
    OBST_ATTRIB(EDGE_STROKE, stroke);
    OBST_PROGRAM_LINK(edge);
    OBST_UNIFORM(EDGE_ORIGIN, origin);
    OBST_UNIFORM(EDGE_SCALE, scale);
    OBST_UNIFORM(EDGE_SCALE_P, scale_p);
    
    OBST_PROGRAM_INIT(arrow);
    OBST_ATTRIB(ARROW_POS, pos);
    OBST_ATTRIB(ARROW_H, h);
    OBST_ATTRIB(ARROW_FILL, fill);
    OBST_PROGRAM_LINK(arrow);
    OBST_UNIFORM(ARROW_ORIGIN, origin);
    OBST_UNIFORM(ARROW_SCALE, scale);

    OBST_PROGRAM_INIT(text);
    OBST_ATTRIB(TEXT_POS, pos);
    OBST_ATTRIB(TEXT_TPOS, tpos);
    OBST_ATTRIB(TEXT_FILL, fill);
    OBST_PROGRAM_LINK(text);
    OBST_UNIFORM(TEXT_ORIGIN, origin);
    OBST_UNIFORM(TEXT_SCALE, scale);
    OBST_UNIFORM(TEXT_SAMPLER, sampler);

    OBST_PROGRAM_INIT(rect);
    OBST_ATTRIB(RECT_POS, pos);
    OBST_ATTRIB(RECT_FILL, fill);
    OBST_PROGRAM_LINK(rect);
    OBST_UNIFORM(RECT_ORIGIN, origin);
    OBST_UNIFORM(RECT_SCALE, scale);

    OBST_GEN_BUFFERS(bdd, BDD);
    OBST_GEN_BUFFERS(edge, EDGE);
    OBST_GEN_BUFFERS(arrow, ARROW);
    OBST_GEN_BUFFERS(text, TEXT);
    OBST_GEN_BUFFERS(rect, RECT);
    OBST_GEN_BUFFERS(ui, UI);
    
    glClearColor(0.96f, 0.96f, 0.96f, 1.f);
}

// These exist only once. Some of the lower functions do not assume that, but many UI functions do.
// @Cleanup: Maybe refactor a few functions to not use these
Opengl_context global_context;
Bdd_store global_store;
Array_t<Bdd_layout> global_layouts;

void opengl_draw_rect(Opengl_context* context, float x1, float y1, float w, float h, u8* fill, float z=0.45f) {
    float x2 = x1+w, y2 = y1+h;
    
    array_append(&context->buf_rect_pos, {
        x1, y1, z, x2, y1, z, x2, y2, z, x1, y1, z, x2, y2, z, x1, y2, z
    });
    for (s64 i = 0; i < 6; ++i) {
        array_append(&context->buf_rect_fill, {fill, 4});
    }
}

// Draw a single node
void opengl_draw_bdd(Opengl_context* context, Bdd_attr a) {
    float x0 = a.x - a.rx * 1.1f;
    float y0 = a.y - a.ry * 1.1f;
    float x1 = a.x + a.rx * 1.1f;
    float y1 = a.y + a.ry * 1.1f;
    array_append(&context->buf_bdd_pos, {
        x0, y0, x1, y1, x0, y1, x0, y0, x1, y0, x1, y1
    });
    for (s64 i = 0; i < 6; ++i) {
        array_append(&context->buf_bdd_x, {a.x, a.y});
        array_append(&context->buf_bdd_r, {a.rx, a.ry});
        array_append(&context->buf_bdd_stroke, {a.stroke, 4});
        array_append(&context->buf_bdd_border, {a.border});
        array_append(&context->buf_bdd_fill, {a.fill, 4});
    }
}

// Convert the id bdd into a label for the bdd
Array_t<u8> _get_bdd_name(Bdd_store* store, u32 name) {
    if (name == 0) {
        auto s = "unnamed";
        return {(u8*)s, (s64)strlen(s)};
    } else {
        return array_subarray(store->name_data, store->names[name], store->names[name+1]);
    }
}

void _measure_str(Opengl_context* context, Array_t<u8> text, float size, float* w_, float* size_adj_, float* ascent_, bool* small) {
    float fs = 1.f / context->scale * size / context->font_size_max;
    float w = 0.f;
    for (u8 c: text) {
        s64 index = opengl_bddlabel_char_index(c);
        if (index == -1) continue;
        w += std::round(context->text_pos[index].advance) * fs;
    }
    
    bool only_small = true;
    for (u8 c: text) only_small &= (bool)(c & 128);
    
    float size_adj = size;
    float ascent = context->font_ascent * size / context->font_size_max;
    float linoff = context->font_linoff * size / context->font_size_max;
    if (only_small) {
        size_adj *= context->font_small_frac;
        ascent   *= context->font_small_frac;
        linoff   *= context->font_small_frac;
    }

    size_adj -= linoff;
    ascent   -= linoff;
    
    if (w_) *w_ = w;
    if (size_adj_) *size_adj_ = size_adj;
    if (ascent_) *ascent_ = ascent;
    if (small) *small = only_small;
}

void opengl_draw_text(
    Opengl_context* context,
    float x, float y,
    Array_t<u8> text, // Note that only the characters mapped by opengl_bddlabel_char_index will be drawn
    float size, // font size in world-coordinates
    float alpha,
    float align, // 1 is right-aligned, 0 centered, -1 left-aligned
    bool do_draw = true,
    float* x_out = nullptr,
    float* y_out = nullptr
) {
    // Note on font rounding: In my quest to get sharp fonts, I round font sizes and positions to
    // pixels. But that does not look great during transitions. So only the final positions and the
    // final font size (i.e. the maximum size) are rounded, for the others we interpolate as
    // usual. To get the pixels to line up, every transformation between here and the final
    // pixel-coordinates has to be taken into account by the rounding.

    float fs = 1.f / context->scale * size / context->font_size_max;
    float size_total, size_adj, linoff, ascent;
    _measure_str(context, text, size, &size_total, &size_adj, &ascent, nullptr);
    
    x += - size_total * (0.5f + align * 0.5f) - 0.5f / context->scale;
    y += - (ascent - size_adj*0.5f)           - 0.5f / context->scale;
    u8 black[] = {  0,   0,   0, (u8)(alpha * 255.f)};
    u8 grey[]  = {100, 100, 100, (u8)(alpha * 255.f)};

    if (x_out) *x_out = x;
    if (y_out) *y_out = y;

    if (not do_draw) return;
    
    for (u8 c: text) {
        s64 index = opengl_bddlabel_char_index(c);
        if (index == -1) continue;
        Text_box box = context->text_pos[index];
        bool draw_light;
        opengl_bddlabel_index_utf8(index, &draw_light);
        
        array_append(&context->buf_text_pos, {
            x+box.x0*fs, y-box.y0*fs, x+box.x1*fs, y-box.y0*fs, x+box.x1*fs, y-box.y1*fs,
            x+box.x0*fs, y-box.y0*fs, x+box.x1*fs, y-box.y1*fs, x+box.x0*fs, y-box.y1*fs
        });
        array_append(&context->buf_text_tpos, {
            box.s0, box.t0, box.s1, box.t0, box.s1, box.t1, box.s0, box.t0, box.s1, box.t1, box.s0, box.t1
        });
        for (s64 i = 0; i < 6; ++i) {
            array_append(&context->buf_text_fill, {draw_light ? grey : black, 4});
        }

        x += std::round(box.advance) * fs;
    }
}

void opengl_draw_text_bdd(
    Opengl_context* context,
    Bdd_store* store,
    Bdd_attr a,
    float fac,
    bool do_draw = true,
    float* x1_out = nullptr,
    float* y1_out = nullptr,
    float* x2_out = nullptr,
    float* y2_out = nullptr
) {
    Array_t<u8> str = _get_bdd_name(store, a.name);
    float size = context->font_size_max * fac;

    float w, size_adj, ascent;
    bool small;
    _measure_str(context, str, size, &w, &size_adj, nullptr, &small);
    u8 fill[] = {a.fill[0], a.fill[1], a.fill[2], (u8)(a.fill[3] * 0.75f * a.name_alpha)};

    float pad = 0.01f;
    float line_fac = 0.6f;
    
    bool oneline = true;
    if (w > 1.93f*a.rx) {
        if (small) {
            oneline = false;
        } else if (do_draw) {
            opengl_draw_rect(context, a.font_x - w/2.f - pad, a.font_y - size_adj/2.f - pad, w + 2.f*pad, size_adj + 2.f*pad, fill);
        }
    }

    auto alnum = [](u8 c) {
        return ('0' <= c and c <= '9') or ('a' <= c and c <= 'z')
            or ('A' <= c and c <= 'Z') or (22  <= c and c <= 31);
    };
        
    
    float alpha = a.stroke[3]/255.f * a.name_alpha;
    if (oneline) {
        opengl_draw_text(context, a.font_x, a.font_y, str, size, alpha, 0.f, do_draw, x1_out, y1_out);
        if (x2_out) *x2_out = 0.f;
        if (y2_out) *y2_out = 0.f;
    } else {
        float fs = fac / context->scale;
        s64 best = str.size;
        float best_val = INFINITY;
        float best_wn = w;
        float wn = 0.f;
        for (s64 i = 0; i+1 < str.size; ++i) {
            u8 c0 = str[i  ] & 127;
            u8 c1 = str[i+1] & 127;
            float val = alnum(c0) and alnum(c1) ? 0.13f :
                c0 == '(' ? 0.08f :
                c0 == '~' ? 0.08f :
                c1 == ')' ? 0.08f :
                c1 == ',' ? 0.05f :
                _is_operator(c0) ? 0.01f : 0.0f;
                
            s64 index = opengl_bddlabel_char_index(str[i]);
            if (index == -1) continue;
            wn += std::round(context->text_pos[index].advance) * fs;

            val += std::abs(wn - w/2.f) / w;
            if (val < best_val) {
                best = i+1;
                best_val = val;
                best_wn = wn;
            }
        }
        float yoff = best < str.size ? size_adj * line_fac : 0.f;
        opengl_draw_text(context, a.font_x,  a.font_y  + yoff, array_subarray(str, 0,    best    ), size, alpha, 0.f, do_draw, x1_out, y1_out);
        opengl_draw_text(context, a.font_x2, a.font_y2 - yoff, array_subarray(str, best, str.size), size, alpha, 0.f, do_draw, x2_out, y2_out);
        if (do_draw) {
            opengl_draw_rect(context, a.font_x - (w-best_wn)/2.f - pad, a.font_y - size_adj/2.f - yoff - pad, w-best_wn + 2.f*pad, size_adj + 2.f*pad, fill);
            opengl_draw_rect(context, a.font_x - best_wn/2.f - pad, a.font_y - size_adj/2.f + yoff - pad, best_wn + 2.f*pad, size_adj + 2.f*pad, fill);
        }
    }
}

// See the note on font rounding above.
void opengl_bdd_text_round(Opengl_context* context, Bdd_store* store, Bdd_attr* a, float fac) {
    a->font_x  = a->x;
    a->font_y  = a->y;
    a->font_x2 = a->x;
    a->font_y2 = a->y;
    
    float x1, y1, x2, y2;
    opengl_draw_text_bdd(context, store, *a, fac, false, &x1, &y1, &x2, &y2);

    auto frac = [](float f) { return std::round(f) - f; };
    
    a->font_x  += frac((x1 - context->origin_x) * context->scale) / context->scale;
    a->font_y  += frac((y1 - context->origin_y) * context->scale) / context->scale;
    a->font_x2 += frac((x2 - context->origin_x) * context->scale) / context->scale;
    a->font_y2 += frac((y2 - context->origin_y) * context->scale) / context->scale;
}

// Calculates the length of the quadratic spline with control points p0, p1 and p2 using numerical
// integration. This is included for reference only, the other code uses the closed form solution
// instead.
float _spline_length_quadrature(Pos p0, Pos p1, Pos p2, s64 n = 50) {
    // Alright, some nontrivial math follows. Basically, this is just numerical integration using
    // Simpson's rule. Doing simple transformations, you can derive that the length of a quadratic
    // spline has the form
    //     integrate[0:1] sqrt(b2*t**2 + b1*t + b0) dt
    // which is difficult to simplify further. So I just calculate the coefficients and do the
    // integration numerically. You can tweak n to get more/less precise results, but this function
    // is not really performance sensitive.
    
    assert(n % 2 == 0);
    float a1 = 2.f * (p1.x - p0.x);
    float a3 = 2.f * (p1.y - p0.y);
    float a2 = 2.f * (p0.x - 2*p1.x + p2.x);
    float a4 = 2.f * (p0.y - 2*p1.y + p2.y);
    float b0 = a1*a1 + a3*a3;
    float b1 = 2*(a1*a2 + a3*a4);
    float b2 = a2*a2 + a4*a4;
    float dt = 1 / (float)n;
    float t = 0.f;
    float l = std::sqrt((b2*t + b1)*t + b0);
    t += dt;
    for (s64 i = 1; i+1 < n; i += 2) {
        l += std::sqrt((b2*t + b1)*t + b0) * 4.f;
        t += dt;
        l += std::sqrt((b2*t + b1)*t + b0) * 2.f;
        t += dt;
    }
    l += std::sqrt((b2*t + b1)*t + b0) * 4.f;
    t += dt;
    l += std::sqrt((b2*t + b1)*t + b0);
    l /= (float)n * 3.f;
    return l;
}

// Calculates the length of the quadratic spline with control points p0, p1 and p2.
float spline_length(Pos p0, Pos p1, Pos p2) {
    // In _spline_length_quadrature I mentioned that the term was difficult to simplify. Still, you
    // can solve it analytically and derive a closed form solution, see e.g. [1] for the formula, or
    // [2] for a derivation.
    //
    // Doing some basic measurements, this is about 6x faster than the above, but there is a large
    // margin of error due to running in the browser. So really it just is a matter of preference.
    //
    // [1] Sablonnière, Paul. "Some approximate methods for computing arc lengths based on quadratic
    //  and cubic spline interpolation or quasi-interpolation." Rend. Sem. Mat. Univ. Politec. Torino
    //  69.1 (2011): 1-20.
    //  https://pdfs.semanticscholar.org/5c80/812405993a9f01c762baf92c893917146201.pdf
    // [2] http://web.archive.org/web/20180831125226/http://www.malczak.linuxpl.com/blog/quadratic-bezier-curve-length/
    
    Pos d0 = {p1.x - p0.x, p1.y - p0.y};
    Pos d1 = {p2.x - p1.x, p2.y - p1.y};

    if (std::abs(d0.x*d1.y - d0.y*d1.x) < 0.001) {
        // Points colinear, the formula below does not work there
        Pos d2 = {p2.x - p0.x, p2.y - p0.y};
        return std::sqrt(d2.x*d2.x + d2.y*d2.y);
    }
    
    Pos dd = {d1.x - d0.x, d1.y - d0.y};
    float a0 = d0.x*d0.x + d0.y*d0.y;
    float a1 = d0.x*d1.x + d0.y*d1.y;
    float a2 = d1.x*d1.x + d1.y*d1.y;
    float da0 = d0.x*dd.x + d0.y*dd.y;
    float da1 = d1.x*dd.x + d1.y*dd.y;
    float dda = dd.x*dd.x + dd.y*dd.y;
    return (a0*a2-a1*a1) / std::sqrt(dda*dda*dda) * std::log(std::abs((da1 + std::sqrt(a2*dda)) / (da0 + std::sqrt(a0*dda))))
        + (da1*std::sqrt(a2) - da0*std::sqrt(a0)) / dda;
}

// Calculates the length of a spline in the interval [0, t]
float spline_length_at(Pos p0, Pos p1, Pos p2, float t) {
    // Split the spline and calculate the length of the subspline
    Pos p1_ = _pos_mix(p0, p1, t);
    Pos p2_ = _pos_mix(p1_, _pos_mix(p1, p2, t), t);
    return spline_length(p0, p1_, p2_);
}

// Calculates coefficients for an approximation of the t -> length mapping used in the edge fragment
// shader to draw gaps of the same size.
void spline_length_approx(Pos p0, Pos p1, Pos p2, float* f1, float* f2) {
    // Note on the spline length approximation: The speed we traverse the spline with varies
    // depending on t and the control points, so the length is not linear in terms of t. But we need
    // an estimate for the length depending on t to draw accurate gaps in the spline. The solution I
    // chose was to approximate the t -> length function with a third-degree interpolation
    // polynomial. This thing is normalised, so that the total length of the curve is 1 (hence both
    // functions map [0,1] -> [0,1]).
    //  I choose four equidistant points to interpolate, t = 0, 1/3, 2/3, 1. The lengths are
    // calculated by numerical integration, as in spline_length. Let f be out interpolation polynomial:
    //     f(x) = f3 * x**3 + f2 * x**2 + f1 * x + f0
    //     f(0) = 0  =>  f0 = 0
    //     f(1) = 1  =>  f3 = 1 - f1 - f2
    //     f(1/3) = l1
    //     f(2/3) = l2
    // where l1, l2 are the lengths at t = 1/3 and t = 2/3.
    //  The only thing left to do is to calculate f1, f2 as some affine function of l1, l2, the
    // constants of which you can derive on paper without too much difficulty.

    float l = spline_length(p0, p1, p2);
    float l1 = spline_length_at(p0, p1, p2, 1.f/3.f) / l;
    float l2 = spline_length_at(p0, p1, p2, 2.f/3.f) / l;
    *f1 =    9.f*l1 -  4.5f*l2 + 1.f;
    *f2 = -22.5f*l1 + 18.f *l2 - 4.5f;
}

// A variant on the numerical integration. This one find the t s.t. the spline has the specified
// length at that point. Could use the closed form one with binary search, but I think this is more
// efficient. (But did not measure! Beware! Mostly this code is older than the closed form code.)
float spline_find_offset(Pos p0, Pos p1, Pos p2, float length, s64 n = 50) {
    assert(n % 2 == 0);
    float a1 = 2.f * (p1.x - p0.x);
    float a3 = 2.f * (p1.y - p0.y);
    float a2 = 2.f * (p0.x - 2*p1.x + p2.x);
    float a4 = 2.f * (p0.y - 2*p1.y + p2.y);
    float b0 = a1*a1 + a3*a3;
    float b1 = 2*(a1*a2 + a3*a4);
    float b2 = a2*a2 + a4*a4;
    float dt = 1 / (float)n;
    float t = 0.f;
    length *= (float)n * 3.f;
    float c = std::sqrt((b2*t + b1)*t + b0);
    if (length <= 0.f) return 0.f;
    float l = 0;
    for (s64 i = 1; i < n; i += 2) {
        float f1 = std::sqrt((b2*(t+    dt) + b1)*(t+    dt) + b0) * 4.f;
        float f2 = std::sqrt((b2*(t+2.f*dt) + b1)*(t+2.f*dt) + b0);
        c += f1 + f2;

        if (l+c > length) {
            // We want to ensure that this function always returns a result in [0,1]
            return std::min(t + (length - l) / c * 2.f*dt, 1.f);
        }
        l += c;
        c = f2;
        t += 2.f*dt;
    }
    return 1.f;
}

// Draws a quadratic spline. Set dash_length to 0 if you do not want dashes. dash_length is in t and
// describes the distance from the start of one dash to the start of the next.
void opengl_draw_edge(
    Opengl_context* context, Pos p0, Pos p1, Pos p2,
    float size, float dash_length, float dash_offset, u8* stroke
) {
    // Note on spline rendering: This is the mathematically most involved part of the
    // program. Basically, the problem is finding the t s.t. the corresponding point on the spline
    // (p0, p1, p2) is closest to p. The technique I use is based on [1, section 4.4], although I
    // have made some significant adjustments to generally simplify the formula and put as much work
    // as possible into the vertex shader. See also the references in [1].
    //  I will not derive the detailed formulae here, but just give a rough overview on how it works
    // and what steps one has to do to end up at my solution.    
    //  Solving the problem exactly requires you to solve third-degree polynomials, which uses cubic
    // roots and all sorts of nastiness. Instead, we proceed by inversion, taking a formula that
    // yields t exactly for a point p _on the curve_. However, the formula is continuous everywhere,
    // and we can get a good enough approximation to t as long as we are close to the curve. For
    // rendering thin lines that is all we need, as points farther away are not drawn anyway.
    //    
    //  The basic steps are as follows:
    //     1. Take a function f that has f(x) = 0 iff x is on the curve
    //     2. Do a first-order approximation of f based on p: f1(x) := f(p) + Df(p)x
    //     3. The set f1 = 0 forms a line
    //     4. Find the closest point to p on that line, p' := p - f(p) Df(p) / dot(Df(p), Df(p))
    //        (D is the total derivative, or, in this case, the transposed gradient.)
    //     5. Apply the inversion formula to p' to get t
    //
    // Regarding f: Look at the paper to get the precise definition, but basically
    //     f(x) = b(x) d(x) + a(x)**2
    // with a, b, d being affine functions mapping into the real numbers.
    //  The inversion formula in step 5 is also in the paper, but it ends up being much simpler in
    // my version, so I will not give the general form here.
    //
    //  Mainly, I did one thing to simplify the problem: Solve it for the spline ((0,0), (1/2,0),
    // (0,1)) (which I will call 'base spline' in the following) by hand and then do a linear
    // transformation to apply that result to the general case. f becomes the function
    //     f(x,y) = y - (x+y)**2
    // and
    //     Df(x,y) = (-2(x+y), 1-2(x+y))
    // Let h be the function mapping t to the position of the point on the curve, i.e.
    //     h(t) = (t(1-t), t**2)
    // It is not difficult to see that, for any point (x,y) on the curve, x+y = t. As a matter of
    // fact, the inversion formula in the paper reduces to precisely this.
    //
    //  Great, so we now know how to solve the problem for the base spline. To map any general
    // spline, first apply a translation to get p0 = 0 (this does not affect any calculations, so
    // just assume this was already the case). Then, choose a matrix A s.t. A p1 = (1/2, 0) and A p2
    // = (0, 1). Let fA be the function f for the base spline, and fG the general one from above, for
    // the spline we are interested in.) Conveniently,
    //     det(A)**2 fA(A x) = fG(x)
    // (This holds due to the special structure of a, b and d.) To get p', the constant does not
    // matter. Additionally, it follows that
    //     DfG(x) = det(A)**2 DfA(A x) A 
    // Finally, to get t we calculate A p' via
    //     A p' = A p - A fA(A p) (DfA(A x) A) / dot(DfA(A x) A, DfA(A x) A)
    // and sum its coordinates.
    //     t = (1 1) A p'
    //       = (1 1) A p - (1 1) A fA(A p) (DfA(A x) A) / dot(DfA(A x) A, DfA(A x) A)
    //       = (1 1) A p - fA(A p) ((1 1) A (DfA(A x) A)) / dot(DfA(A x) A, DfA(A x) A)
    // Letting
    //     (x,y) := A p
    //     z := x + y
    //     dg := transpose(DfA(A x) A) = transpose(A) (-2z, 1-2z)
    //     w := (1 1) A dg
    // the above term becomes
    //       = (x+y) - (y - (x+y)**2) ((1 1) A dg) / dot(dg, dg)
    //       = z - (y - z**2) w / dot(dg, dg)
    // Note that all the constants I introduced are linear functions of p and can thus be calculated
    // in the vertex shader and then linearly interpolated. So the last line really is the only
    // thing the fragment shader has to do to get t.
    //
    //  One small wrinkle of this technique (also present in the original version): If p0, p1, p2 are
    // colinear, things are not well-defined. As a small exercise, you may want to look at the
    // description above and find out where exactly things break down, as it is a bit implicit. I'll
    // wait.
    //  Back again? Great. As you no doubt noticed, the A is not well-defined in that case. Fixing
    // the problem is easy, however, it suffices to add a small perturbation to p1 so that the
    // points are not colinear anymore. (Also, move it to the middle of p0, p2 beforehand, the
    // approximation gets worse the closer p1 is to one of them.)
    //
    // [1] http://research.microsoft.com/en-us/um/people/hoppe/ravg.pdf
    // Nehab, Diego, and Hugues Hoppe. "Random-access rendering of general vector graphics."
    // ACM Transactions on Graphics (TOG) 27.5 (2008): 135.
    //
    // After finding a t, the rest is quite simple. We get the distance from the point at t, decide
    // whether to render or discard (points near the edges are antialiased). Then we do some scaling
    // and use the approximation to get the length of the curve until t and use that to decide
    // whether to make a gap or not.
    
    float ux = p0.y - p2.y;
    float uy = p2.x - p0.x;
    float f = 1.f / std::sqrt(ux*ux + uy*uy);
    ux *= f; uy *= f;
    float d = (p1.x - p0.x)*ux + (p1.y - p0.y)*uy;
    if (std::abs(d) < 0.001) {
        // The approximation for the bezier curve is not well defined if the points are colinear. So
        // perturb them a bit. Also put p1 into the middle, as the approximation work better
        // then. (It is bad mostly if p1 is near p0 or p2.)
        p1.x = 0.5f*(p0.x + p2.x) + ux * 0.001;
        p1.y = 0.5f*(p0.y + p2.y) + uy * 0.001;
    } else if (d < 0) {
        ux *= -1.f; uy *= -1.f;
    }
    float f2 = 2.f*size + 2.f/context->scale;
    ux *= f2; uy *= f2;

    float a0, a1;
    spline_length_approx(p0, p1, p2, &a0, &a1);

    // No dashes are just a special case of dashes
    if (dash_length == 0.f) {
        dash_offset = 0.f;
        dash_length = 3.f;
    }

    float vx = (p2.x - p0.x) * f * 2.f/context->scale;
    float vy = (p2.y - p0.y) * f * 2.f/context->scale;

    // We want a triangle enclosing the curve on all sides. I am going to be lazy here and assume
    // the curve is 'nice', i.e. p1 is 'between' p0 and p2.
    
    array_append(&context->buf_edge_pos, {
        p1.x + ux     , p1.y + uy,
        p1.x + ux     , p1.y + uy,
        p0.x + ux - vx, p0.y + uy - vy,
        p2.x + ux + vx, p2.y + uy + vy,
        p0.x - ux - vx, p0.y - uy - vy,
        p2.x - ux + vx, p2.y - uy + vy,
        p2.x - ux + vx, p2.y - uy + vy,
    });
    for (s64 i = 0; i < 7; ++i) {
        array_append(&context->buf_edge_p0, {p0.x, p0.y});
        array_append(&context->buf_edge_p1, {p1.x, p1.y});
        array_append(&context->buf_edge_p2, {p2.x, p2.y});
        array_append(&context->buf_edge_a,  {a0,   a1  });
        array_append(&context->buf_edge_dash, {size, dash_offset, dash_length});
        array_append(&context->buf_edge_stroke, {stroke, 4});
    }
}

// Simpler interface for debug functionality
void opengl_draw_edge_simple(Opengl_context* context, Pos p0, Pos p2, float size, u32 stroke) {
    Pos p1 {(p0.x+p2.x)*0.5f, (p0.y+p2.y)*0.5f};
    u8 stroke_arr[4] = {(u8)(stroke >> 24 & 0xff), (u8)(stroke >> 16 & 0xff), (u8)(stroke >> 8 & 0xff), (u8)(stroke & 0xff)};
    opengl_draw_edge(context, p0, p1, p2, size, 0.f, 0.f, stroke_arr);
}

// Draws an arrow with the tip at p1, coming from the direction of p0.
void opengl_draw_arrow(Opengl_context* context, Pos p0, Pos p1, float size, u8* fill) {
    float ux = p1.x - p0.x;
    float uy = p1.y - p0.y;
    float f = (size + 0.5f/context->scale) / std::sqrt(ux*ux + uy*uy);
    ux *= f; uy *= f;

    Pos p2 {p1.x - ux - 0.5f*uy, p1.y - uy + 0.5f*ux};
    Pos p3 {p1.x - ux + 0.5f*uy, p1.y - uy - 0.5f*ux};
    Pos pc {(p1.x + p2.x + p3.x) / 3.f, (p1.y + p2.y + p3.y) / 3.f};
    array_append(&context->buf_arrow_pos, {
        p1.x, p1.y, p2.x, p2.y, pc.x, pc.y,
        p2.x, p2.y, p3.x, p3.y, pc.x, pc.y, 
        p3.x, p3.y, p1.x, p1.y, pc.x, pc.y, 
    });

    auto get_h = [context](Pos p1, Pos p2, Pos pc) {
        Pos v {p1.y-p2.y, p2.x-p1.x};
        float vf = context->scale / std::sqrt(v.x*v.x + v.y*v.y);
        v.x *= vf; v.y *= vf;
        return v.x*((p1.x+p2.x)*0.5f - pc.x) + v.y*((p1.y+p2.y)*0.5f - pc.y);
    };
    array_append(&context->buf_arrow_h, {
        0.5f, 0.5f, get_h(p1, p2, pc) + 0.5f,
        0.5f, 0.5f, get_h(p2, p3, pc) + 0.5f,
        0.5f, 0.5f, get_h(p3, p1, pc) + 0.5f,
    });
    
    for (s64 i = 0; i < 9; ++i) {
        array_append(&context->buf_arrow_fill, {fill, 4});
    }
}

// Resets the buffers, initialises Opengl state
void opengl_frame_init(Opengl_context* context) {
    glClear(GL_COLOR_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);
    
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glDepthFunc(GL_LEQUAL);
    
    glActiveTexture(GL_TEXTURE0);
    
    context->buf_bdd_pos.size = 0;
    context->buf_bdd_x.size = 0;
    context->buf_bdd_r.size = 0;
    context->buf_bdd_stroke.size = 0;
    context->buf_bdd_border.size = 0;
    context->buf_bdd_fill.size = 0;
    
    context->buf_edge_pos.size = 0;
    context->buf_edge_p0.size = 0;
    context->buf_edge_p1.size = 0;
    context->buf_edge_p2.size = 0;
    context->buf_edge_a.size = 0;
    context->buf_edge_dash.size = 0;
    context->buf_edge_stroke.size = 0;

    context->buf_arrow_pos.size = 0;
    context->buf_arrow_h.size = 0;
    context->buf_arrow_fill.size = 0;
    
    context->buf_text_pos.size = 0;
    context->buf_text_tpos.size = 0;
    context->buf_text_fill.size = 0;

    context->buf_rect_pos.size = 0;
    context->buf_rect_fill.size = 0;

    context->origin_x = -1.f;
    context->origin_y = -1.f;
    context->scale    = std::min(
        context->width  / (context->layout_max_x + 2.f),
        context->height / (context->layout_max_y + 2.f)
    );
    context->origin_y -= context->height / context->scale - (context->layout_max_y + 2.f);

    if (context->font_regenerate == 1) {
        // If there is nothing there, we have to idea what size to construct the font in. But we
        // also do not need one, so skipping here is fine.
        if (context->not_completely_empty) {
            opengl_text_prepare(context);
        }
        context->font_regenerate = 0;
    } else if (context->font_regenerate > 1) {
        --context->font_regenerate;
    }
}

// Draws the content of all the buffers onto the screen.
void opengl_frame_draw(Opengl_context* context) {

#define OBST_DO_BUFFER(x, name, member, siz, type, norm)                      \
    glBindBuffer(GL_ARRAY_BUFFER, context->buffers_##x[context->name]); \
    glBufferData(GL_ARRAY_BUFFER, context->member.size * sizeof(context->member[0]), context->member.data, GL_STREAM_DRAW); \
    glVertexAttribPointer(context->name, siz, type, norm, 0, 0); \
    glEnableVertexAttribArray(context->name)

#define OBST_DO_UNIFORM(name, type, ...) \
    glUniform##type(context->uniforms[context->name], __VA_ARGS__)

    float ox = context->origin_x + (-context->canvas_x + context->width ) / 2.f / context->scale;
    float oy = context->origin_y + (-context->canvas_y + context->height) / 2.f / context->scale;
    float sx = context->scale * 2.f / context->screen_w;
    float sy = context->scale * 2.f / context->screen_h;

    // There is some juggling involved with the depth buffer and drawing order.
    
    glDisable(GL_DEPTH_TEST);

    glUseProgram(context->program_edge);

    OBST_DO_UNIFORM(EDGE_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(EDGE_SCALE, 2f, sx, sy);
    OBST_DO_UNIFORM(EDGE_SCALE_P, 1f, context->scale);

    OBST_DO_BUFFER(edge, EDGE_POS,    buf_edge_pos,    2, GL_FLOAT, 0);
    OBST_DO_BUFFER(edge, EDGE_P0,     buf_edge_p0,     2, GL_FLOAT, 0);
    OBST_DO_BUFFER(edge, EDGE_P1,     buf_edge_p1,     2, GL_FLOAT, 0);
    OBST_DO_BUFFER(edge, EDGE_P2,     buf_edge_p2,     2, GL_FLOAT, 0);
    OBST_DO_BUFFER(edge, EDGE_A,      buf_edge_a,      2, GL_FLOAT, 0);
    OBST_DO_BUFFER(edge, EDGE_DASH,   buf_edge_dash,   3, GL_FLOAT, 0);
    OBST_DO_BUFFER(edge, EDGE_STROKE, buf_edge_stroke, 4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLE_STRIP, 0, context->buf_edge_pos.size / 2);

    glEnable(GL_DEPTH_TEST);
    glUseProgram(context->program_arrow);
    
    OBST_DO_UNIFORM(ARROW_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(ARROW_SCALE, 2f, sx, sy);

    OBST_DO_BUFFER(arrow, ARROW_POS,  buf_arrow_pos,  2, GL_FLOAT, 0);
    OBST_DO_BUFFER(arrow, ARROW_H,    buf_arrow_h,    1, GL_FLOAT, 0);
    OBST_DO_BUFFER(arrow, ARROW_FILL, buf_arrow_fill, 4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLES, 0, context->buf_arrow_pos.size / 2);
    
    glUseProgram(context->program_bdd);

    OBST_DO_UNIFORM(BDD_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(BDD_SCALE, 2f, sx, sy);
    OBST_DO_UNIFORM(BDD_SCALE_P, 1f, context->scale);

    OBST_DO_BUFFER(bdd, BDD_POS,    buf_bdd_pos,    2, GL_FLOAT, 0);
    OBST_DO_BUFFER(bdd, BDD_X,      buf_bdd_x,      2, GL_FLOAT, 0);
    OBST_DO_BUFFER(bdd, BDD_R,      buf_bdd_r,      2, GL_FLOAT, 0);
    OBST_DO_BUFFER(bdd, BDD_STROKE, buf_bdd_stroke, 4, GL_UNSIGNED_BYTE, 1);
    OBST_DO_BUFFER(bdd, BDD_BORDER, buf_bdd_border, 1, GL_FLOAT, 0);
    OBST_DO_BUFFER(bdd, BDD_FILL,   buf_bdd_fill,   4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLES, 0, context->buf_bdd_pos.size / 2);

    glUseProgram(context->program_rect);
    
    OBST_DO_UNIFORM(RECT_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(RECT_SCALE, 2f, sx, sy);

    OBST_DO_BUFFER(rect, RECT_POS,  buf_rect_pos,  3, GL_FLOAT, 0);
    OBST_DO_BUFFER(rect, RECT_FILL, buf_rect_fill, 4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLES, 0, context->buf_rect_pos.size / 3);

    glUseProgram(context->program_text);
    glActiveTexture(GL_TEXTURE0);

    OBST_DO_UNIFORM(TEXT_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(TEXT_SCALE, 2f, sx, sy);
    OBST_DO_UNIFORM(TEXT_SAMPLER, 1i, 0);

    OBST_DO_BUFFER(text, TEXT_POS,     buf_text_pos,     2, GL_FLOAT, 0);
    OBST_DO_BUFFER(text, TEXT_TPOS,    buf_text_tpos,    2, GL_FLOAT, 0);
    OBST_DO_BUFFER(text, TEXT_FILL,    buf_text_fill,    4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLES, 0, context->buf_text_pos.size / 2);

    // OBST_DO_BUFFER and OBST_DO_UNIFORM purposely left defined for the platform layer
}

Layout_memory global_layout_memory;

// Add the new frames from store to layouts. Sets max_x, max_y, max_points to their correct values.
void layout_render(Array_t<Bdd_layout>* layouts, float* max_x, float* max_y, s64* max_points, Bdd_store store) {
    constexpr s64 iter_max = 500;
    
#ifndef OBST_DBG_SHOW_FORCE_LAYOUT
    s64 num_frames = store.snapshots.size-1;
    s64 first_new = layouts->size;
    array_resize(layouts, num_frames);

    // Depending on whether there already are frame, we either copy the last state or use a new
    // initial one.
    Bdd_layout layout_cur;
    if (first_new > 0) {
        layout_copy(&layout_cur, (*layouts)[first_new-1]);
        layout_set_id_max(&layout_cur, store.bdd_data.size);
    } else {
        layout_init(&layout_cur, store.bdd_data.size);
    }

    // Go through all the frames backwards, as that makes the final graph look nicer
    for (s64 i = num_frames-1; i >= first_new; --i) {
        auto bdds = array_subarray(
            store.snapshot_data_bdd,
            store.snapshots[i].offset_bdd,
            store.snapshots[i+1].offset_bdd
        );

        // Cheap heuristic: For the first frame, which generally has the largest amount of new
        // nodes, we want to simulate for more steps, as those sometimes do not converge quickly
        // enough.
        layout_graph(bdds, &layout_cur, &global_layout_memory, i == num_frames-1 ? 2*iter_max : iter_max);
        layout_splinify(&layout_cur, &global_layout_memory);
        layout_copy(&(*layouts)[i], layout_cur);
    }
#else
    // This is the debug code
    s64 num_frames = iter_max+1;
    array_resize(layouts, num_frames);
    
    for (s64 i = 0; i < num_frames; ++i) {
        auto bdds = array_subarray(
            store.snapshot_data_bdd,
            store.snapshots[store.snapshots.size-2].offset_bdd,
            store.snapshots[store.snapshots.size-1].offset_bdd
        );

        layout_init(&(*layouts)[i], store.bdd_data.size);
        layout_graph(bdds, &(*layouts)[i], &global_layout_memory, i);
        layout_splinify(&(*layouts)[i], &global_layout_memory);
    }
#endif    

    // Calculate the bounds
    float nx = INFINITY;
    float mx = -INFINITY;
    float my = 0.f;
    for (s64 i = 0; i < layouts->size; ++i) {
        for (Pos_id j: (*layouts)[i].bdd_pos) {
            if (nx > j.x) nx = j.x;
            if (mx < j.x) mx = j.x;
            if (my < j.y) my = j.y;
        }
    }
    
    for (s64 i = 0; i < layouts->size; ++i) {
        for (Pos_id& j: (*layouts)[i].bdd_pos) {
            j.x -= nx;
        }
        for (Pos& j: (*layouts)[i].edge_data) {
            j.x -= nx;
        }
    }
    mx -= nx;
    
    if (max_x) *max_x = mx;
    if (max_y) *max_y = my;

    s64 mp = 0;
    for (s64 i = 0; i < layouts->size; ++i) {
        for (s64 j = 0; j+1 < (*layouts)[i].edges.size; ++j) {
            s64 p = (*layouts)[i].edges[j+1].offset - (*layouts)[i].edges[j].offset;
            if (mp < p) mp = p;
        }
    }
    if (max_points) *max_points = mp;
}

// Interpolates and draws the frame at time using the data from layouts and store.
void layout_frame_draw(Opengl_context* context, Array_t<Bdd_layout> layouts, Bdd_store store, float time) {
    if (time < 0.f) time = 0.f;
    s64 frame = (s64)time;
    float t = time - (s64)frame;
    
    // Check whether we are trying to render a sensible frame
    if (layouts.size == 0) {
        context->buf_attr_cur.size = 0; // these are checked to determine whether to draw the bddinfo hover text
        return;
    }
    if (frame >= layouts.size-1) {
        // Small hack: instead of rendering the last frame and trying to interpolate it with the
        // next (which does not exist) just do the second to last one but set t = 1
        frame = layouts.size - 2;
        t = 1.f;
    }

    // Allocate some memory. A new allocation is only necessary when the bounds change, i.e. a new
    // algorithm execution has been added to the display.
    s64 buf_render_size = store.bdd_data.size * 6 * sizeof(u32) + 2 * context->layout_max_points * sizeof(Pos);
    if (context->buf_render.size < buf_render_size) {
        array_free(&context->buf_render);
        context->buf_render = array_create<u8>(buf_render_size);
    }

    memset(context->buf_render.data, -1, context->buf_render.size);
    u8* buf_render_p = context->buf_render.data;
    Array_t<u32> id_map0   = array_create_from<u32>(&buf_render_p,   store.bdd_data.size);
    Array_t<u32> id_map1   = array_create_from<u32>(&buf_render_p,   store.bdd_data.size);
    Array_t<u32> edge_map0 = array_create_from<u32>(&buf_render_p, 2*store.bdd_data.size);
    Array_t<u32> edge_map1 = array_create_from<u32>(&buf_render_p, 2*store.bdd_data.size);
    Array_dyn<Pos> edge_data = Array_dyn<Pos> {array_create_from<Pos>(&buf_render_p, 2*context->layout_max_points)};
    assert(buf_render_p <= (u8*)context->buf_render.end());

    array_resize(&context->buf_attr_cur, store.bdd_data.size);
    Array_t<Bdd_attr> attr_cur = context->buf_attr_cur;

#ifndef OBST_DBG_SHOW_FORCE_LAYOUT
    auto bdds0 = array_subarray(store.snapshot_data_bdd, store.snapshots[frame  ].offset_bdd, store.snapshots[frame+1].offset_bdd);
    auto bdds1 = array_subarray(store.snapshot_data_bdd, store.snapshots[frame+1].offset_bdd, store.snapshots[frame+2].offset_bdd);
#else
    // For the debug view, we always want to show the last frame
    s64 frame_bdds = store.snapshots.size-2;
    auto bdds0 = array_subarray(store.snapshot_data_bdd, store.snapshots[frame_bdds].offset_bdd,
        store.snapshots[frame_bdds+1].offset_bdd);
    auto bdds1 = bdds0;
#endif
    
    for (u32 i0 = 0; i0 < bdds0.size; ++i0) { id_map0[bdds0[i0].id] = i0; }
    for (u32 i1 = 0; i1 < bdds1.size; ++i1) { id_map1[bdds1[i1].id] = i1; }

    Draw_param param = context->draw_param;
    
    auto color_set = [](u8* into, u32 color) {
        into[0] = color >> 24 & 0xff;
        into[1] = color >> 16 & 0xff;
        into[2] = color >>  8 & 0xff;
        into[3] = color       & 0xff;
    };
    auto color_get = [](u8* from) {
        return from[0] << 24 | from[1] << 16 | from[2] << 8 | from[3];
    };
    auto color_inter = [](u8* a0, u8* a1, float t) {
        a0[0] = (u8)((1.f-t) * (float)a0[0] + t * (float)a1[0]);
        a0[1] = (u8)((1.f-t) * (float)a0[1] + t * (float)a1[1]);
        a0[2] = (u8)((1.f-t) * (float)a0[2] + t * (float)a1[2]);
        a0[3] = (u8)((1.f-t) * (float)a0[3] + t * (float)a1[3]);
    };

    // This returns the base attributes of a bdd from the given layout and Bdd for a specific
    // animation frame.
    auto bdd_attr_get = [param, context, &store, &color_set](Bdd_layout layout, Bdd bdd, Array_t<u32> id_map) {
        Bdd_attr a;
        a.x = layout.bdd_pos[id_map[bdd.id]].x;
        a.y = layout.bdd_pos[id_map[bdd.id]].y;
        a.rx = param.node_radius;
        a.ry = param.node_radius * param.squish_fac;
        a.name = bdd.name;

        opengl_bdd_text_round(context, &store, &a, 1.f);
        
        if (bdd.flags & Bdd::CURRENT) {
            color_set(a.stroke, 0x7f0a13ff);
            color_set(a.fill,   0xf8f2f3ff);
            a.border *= 1.2f;
        } else if (bdd.flags & Bdd::MARKED) { 
            color_set(a.stroke, 0x217516ff);
            color_set(a.fill,   0xf3f8f3ff);
        } else if (bdd.flags & (Bdd::MARKED_E0 | Bdd::MARKED_E1)) { 
            color_set(a.stroke, 0x884babff);
            color_set(a.fill,   0xf9f6fbff);
        } else if (bdd.flags & Bdd::TEMPORARY) {
            color_set(a.stroke, 0x104354ff);
            color_set(a.fill,   0xf3f5f6ff);
        } else {
            color_set(a.stroke, 0x000000ff);
            color_set(a.fill,   0xffffffff);
        }
        if (bdd.flags & Bdd::MARKED_E0) {
            a.mark_child0 = 1.f;
        }
        if (bdd.flags & Bdd::MARKED_E1) {
            a.mark_child1 = 1.f;
        }
        
        return a;
    };
    // Linearly interpolate the attributes of two bdds
    auto bdd_attr_inter = [&color_inter](Bdd_attr* a0, Bdd_attr a1, float t) {
        a0->x  = (1.f-t) * a0->x  + t * a1.x;
        a0->y  = (1.f-t) * a0->y  + t * a1.y;
        a0->rx = (1.f-t) * a0->rx + t * a1.rx;
        a0->ry = (1.f-t) * a0->ry + t * a1.ry;
        a0->font_x = (1.f-t) * a0->font_x + t * a1.font_x;
        a0->font_y = (1.f-t) * a0->font_y + t * a1.font_y;
        a0->font_x2 = (1.f-t) * a0->font_x2 + t * a1.font_x2;
        a0->font_y2 = (1.f-t) * a0->font_y2 + t * a1.font_y2;
        a0->border = (1.f-t) * a0->border + t * a1.border;
        color_inter(a0->stroke, a1.stroke, t);
        color_inter(a0->fill,   a1.fill,   t);
        a0->mark_child0 = (1.f-t) * a0->mark_child0 + t * a1.mark_child0;
        a0->mark_child1 = (1.f-t) * a0->mark_child1 + t * a1.mark_child1;
        if (a0->name == a1.name or a0->name == 0 or a1.name == 0) {
            a0->name_alpha = 1.f;
            a0->name = std::max(a0->name, a1.name);
        } else {
            a0->name = t <= 0.5 ? a0->name : a1.name;
            a0->name_alpha = std::abs(2.f * t - 1.f);
        }
    };
    // Draw the bdds with the given attributes
    auto bdd_attr_apply = [param, context, &store, &attr_cur](Bdd_attr a, u32 id) {
        opengl_draw_bdd(context, a);
        opengl_draw_text_bdd(context, &store, a, a.rx / param.node_radius);
        attr_cur[id] = a;
    };
    // Gives a point on the edge of a bdd. Used to position the children during the creation
    // animation.
    auto bdd_attr_edge = [](float* x, float* y, Bdd_attr a, float vx, float vy) {
        assert(a.rx > 0.f and a.ry > 0.f);
        float len = std::sqrt((vx/a.rx)*(vx/a.rx) + (vy/a.ry)*(vy/a.ry));
        assert(len > 0.f);
        *x = vx / len + a.x;
        *y = vy / len + a.y;
    };

    for (u32 i = 1; i < store.bdd_data.size; ++i) {
        if (id_map0[i] != -1 and id_map1[i] != -1) {
            // Bdd existed in both frames. Just simple interpolation here.

            Bdd bdd0 = bdds0[id_map0[i]];
            Bdd bdd1 = bdds1[id_map1[i]];
            Bdd_attr a0 = bdd_attr_get(layouts[frame],  bdd0, id_map0);
            Bdd_attr a1 = bdd_attr_get(layouts[frame+1], bdd1, id_map1);
            bdd_attr_inter(&a0, a1, t);
            bdd_attr_apply(a0, i);
        } else if (id_map0[i] == -1 and id_map1[i] != -1) {
            // Bdd did not exist in the earlier frame, but does exist afterwards. We need to draw
            // the creation animation.
            
            Bdd bdd1 = bdds1[id_map1[i]];
            Bdd_attr a1 = bdd_attr_get(layouts[frame+1], bdd1, id_map1);

            // Find a parent. Due to the way the algorithms work, this is also the only parent.
            u32 j = -1;
            for (u32 jj = 0; jj < bdds1.size; ++jj) {
                if (bdds1[jj].child0 == i or bdds1[jj].child1 == i) {
                    j = bdds1[jj].id; break;
                }
            }

            // If the bdd is created at the same time as some of its children, we draw a different
            // animation. Also, we need to ensure there is a parent. (That is not the case when we
            // start a new algorithm and a new root is created.)
            Bdd_attr a0;
            if (j != -1 and id_map0[j] != -1 and not bdd1.child0 and not bdd1.child1) {
                // Draw the animation where the bdd 'pops' out of its parent. The child is
                // positioned on the edge of the parent here.
                Bdd_attr b0 = bdd_attr_get(layouts[frame], bdds0[id_map0[j]], id_map0);
                bdd_attr_edge(&a0.x, &a0.y, b0, a1.x - b0.x, a1.y - b0.y);
                a0.font_x = a0.x;
                a0.font_y = a0.y;
                a0.font_x2 = a0.x;
                a0.font_y2 = a0.y;
            } else {
                // Just have the bdd start at its new position
                a0.x = a1.x;
                a0.y = a1.y;
                a0.font_x = a1.font_x;
                a0.font_y = a1.font_y;
                a0.font_x2 = a1.font_x2;
                a0.font_y2 = a1.font_y2;
            }
            // In both cases we want the bdd to become larger during the animation
            a0.rx = 0.f;
            a0.ry = 0.f;
            color_set(a0.stroke, color_get(a1.stroke) & ~0xff);
            color_set(a0.fill  , color_get(a1.fill  ) & ~0xff);
            
            bdd_attr_inter(&a0, a1, t);
            bdd_attr_apply(a0, i);
        } else if (id_map0[i] != -1 and id_map1[i] == -1) {
            // Bdd did exist before but is gone in the second animation frame. Draw a vanishing
            // animation by letting it fade out.
            Bdd_attr a0 = bdd_attr_get(layouts[frame], bdds0[id_map0[i]], id_map0);
            Bdd_attr a1 = a0;
            color_set(a1.stroke, color_get(a1.stroke) & ~0xff);
            color_set(a1.fill  , color_get(a1.fill  ) & ~0xff);
            bdd_attr_inter(&a0, a1, t);
            bdd_attr_apply(a0, i);
        } else {
            // bdd existed neither before nor after, so do nothing
        }
    }

    // Edge ids are just the parent and then one bit of edge type.
    for (s64 i = 0; i+1 < layouts[frame].edges.size; ++i) {
        Edge edge = layouts[frame].edges[i];
        u32 edge_id = bdds0[edge.from].id << 1 | (edge.type-1);
        edge_map0[edge_id] = i;
    }
    for (s64 i = 0; i+1 < layouts[frame+1].edges.size; ++i) {
        Edge edge = layouts[frame+1].edges[i];
        u32 edge_id = bdds1[edge.from].id << 1 | (edge.type-1);
        edge_map1[edge_id] = i;
    }
    
    // Any subcurve of a quadratic spline is once again a quadratic spline. (Not entirely obvious,
    // but very useful.) This splits the spline at a specific t.
    auto spline_split_t = [](Pos out[3], Pos p0, Pos p1, Pos p2, float t) {
        assert(0.f <= t and t <= 1.f);
        out[0] = _pos_mix(p0, p1, t);
        out[2] = _pos_mix(p1, p2, t);
        out[1] = _pos_mix(out[0], out[2], t);
    };
    // Same as above, but split at a specific y
    auto spline_split_y = [&spline_split_t](Pos out[3], Pos p0, Pos p1, Pos p2, float y) {
        float t = (y - p0.y) / (p2.y - p0.y); // @Cleanup Could solve equation here
        spline_split_t(out, p0, p1, p2, t);
    };

    // Interpolate the splines in edge0_data and edge1_data
    auto edge_mix_points = [param, &spline_split_y](
        Array_dyn<Pos>* out, Array_t<Pos> edge0_data, Array_t<Pos> edge1_data, float t
    ) {
        // The basic idea is simple: We want to go through the points in direction of y and
        // interpolate the ones on the same height. If, at some height, only one spline has a point,
        // then we split the other to introduce one there as well.
        array_push_back(out, _pos_mix(edge0_data[0], edge1_data[0], t));

        float y00 = edge0_data[0].y;
        float y01 = edge1_data[0].y;
        float y0t = _pos_mix(edge0_data[0], edge1_data[0], t).y;
        float y10 = edge0_data[edge0_data.size-1].y;
        float y11 = edge1_data[edge1_data.size-1].y;
        float y1t = _pos_mix(edge0_data[edge0_data.size-1], edge1_data[edge1_data.size-1], t).y;
        float sy0 = (y1t - y0t) / (y10 - y00);
        float sy1 = (y1t - y0t) / (y11 - y01);
        float oy0 = y0t - y00 * sy0;
        float oy1 = y0t - y01 * sy1;
        auto edge0 = [&edge0_data, sy0, oy0](s64 i) { return Pos {edge0_data[i].x, edge0_data[i].y * sy0 + oy0}; };
        auto edge1 = [&edge1_data, sy1, oy1](s64 i) { return Pos {edge1_data[i].x, edge1_data[i].y * sy1 + oy1}; };
        
        s64 i0 = 0;
        s64 i1 = 0;
        Pos p00 = edge0(i0  );
        Pos p01 = edge0(i0+1);
        Pos p10 = edge1(i1  );
        Pos p11 = edge1(i1+1);
        while (true) {
            if (std::abs(edge0(i0+2).y - edge1(i1+2).y) < param.edge_point_merge) {
                // Both splines have a point, merge
                array_append(out, {
                    _pos_mix(p01, p11, t),
                    _pos_mix(edge0(i0+2), edge1(i1+2), t)
                });
                i0 += 2;
                i1 += 2;
                if (i0+2 >= edge0_data.size or i1+2 >= edge1_data.size) break;
                p00 = edge0(i0  );
                p01 = edge0(i0+1);
                p10 = edge1(i1  );
                p11 = edge1(i1+1);
            } else if (edge0(i0+2).y < edge1(i1+2).y) {
                // edge0 has a point here, so split edge1
                Pos m[3];
                spline_split_y(m, p00, p01, edge0(i0+2), edge1(i1+2).y);

                array_append(out, {
                    _pos_mix(m[0], p11, t),
                    _pos_mix(m[1], edge1(i1+2), t)
                });
                i1 += 2;
                p00 = m[1];
                p01 = m[2];
                p10 = edge1(i1  );
                p11 = edge1(i1+1);
            } else {
                // edge1 has a point here, so split edge0
                Pos m[3];
                spline_split_y(m, p10, p11, edge1(i1+2), edge0(i0+2).y);
                array_append(out, {
                    _pos_mix(p01, m[0], t),
                    _pos_mix(edge0(i0+2), m[1], t)
                });
                i0 += 2;
                p00 = edge0(i0  );
                p01 = edge0(i0+1);
                p10 = m[1];
                p11 = m[2];
            }
        }
        assert(i0+1 == edge0_data.size and i1+1 == edge1_data.size);
    };

    // Modify the spline so that it starts on the edge of the bdd. This assumes that (a.x, a.x) is
    // the same as the first control point. (So the spline starts in the centre of the bdd.)
    auto edge_bdd_edge = [](Pos* o0, Pos* o1, Pos* o2, Bdd_attr a) {
        Pos p0 = {o0->x / a.rx, o0->y / a.ry};
        Pos p1 = {o1->x / a.rx, o1->y / a.ry};
        Pos p2 = {o2->x / a.rx, o2->y / a.ry};
        p1.x -= p0.x; p1.y -= p0.y;
        p2.x -= p0.x; p2.y -= p0.y;

        // Do binary search to find the position that has distance 1 from p0 in ellipse coordinates
        float t0 = 0.f;
        float t1 = 1.f;
        for (s64 i = 0; i < 10; ++i) {
            float t = (t0 + t1) * 0.5f;
            // This is just the quadratic spline evaluation for the special case of p0 = 0
            Pos p {2*p1.x*t*(1.f-t)+p2.x*t*t, 2*p1.y*t*(1.f-t)+p2.y*t*t};
            if (p.x*p.x + p.y*p.y > 1.f) {
                t1 = t;
            } else {
                t0 = t;
            }
        }

        // Do a second binary search to find the position that lies outside the border.
        t0 = (t0 + t1) * 0.5f;
        t1 = 1.f;
        // For the border we want to evaluate distance in world space again
        p1.x *= a.rx; p1.y *= a.ry;
        p2.x *= a.rx; p2.y *= a.ry;
        Pos pp {2*p1.x*t0*(1.f-t0)+p2.x*t0*t0, 2*p1.y*t0*(1.f-t0)+p2.y*t0*t0};
        float bwidth = 0.3 * a.border * a.rx; // The 0.3 should be a 0.5 to be exactly on the border, but pointing slightly inside looks better.
        bwidth *= bwidth;
        for (s64 i = 0; i < 10; ++i) {
            float t = (t0 + t1) * 0.5f;
            // This is just the quadratic spline evaluation for the special case of p0 = 0
            Pos p {2*p1.x*t*(1.f-t)+p2.x*t*t - pp.x, 2*p1.y*t*(1.f-t)+p2.y*t*t - pp.y};
            if (p.x*p.x + p.y*p.y > bwidth) {
                t1 = t;
            } else {
                t0 = t;
            }
        }

        // Split the spline
        float t = (t0 + t1) * 0.5f;
        Pos oo = _pos_mix(*o1, *o2, t);
        *o0 = _pos_mix(*o0, *o1, t);
        *o0 = _pos_mix(*o0, oo, t);
        *o1 = oo;
    };

    // Modify the spline so that it starts at b and ends at d by doing a linear transformation
    auto edge_project = [](Array_t<Pos>* edge_data, Pos b, Pos d) {
        Pos a = (*edge_data)[0];
        Pos c = (*edge_data)[edge_data->size-1];
        Pos e = {(a.x + c.x) * 0.5f, (a.y + c.y) * 0.5f};
        Pos f = {(b.x + d.x) * 0.5f, (b.y + d.y) * 0.5f};
        a.x -= e.x; a.y -= e.y;
        b.x -= f.x; b.y -= f.y;
        
        float g = (b.x - a.x) / a.y;
        float h = b.y / a.y;
        for (s64 i = 0; i < edge_data->size; ++i) {
            Pos p = (*edge_data)[i];
            p.x -= e.x; p.y -= e.y;
            p.x += g * p.y;
            p.y *= h;
            p.x += f.x; p.y += f.y;
            (*edge_data)[i] = p;
        }
    };

    // Draw the spline stored in edge_data
    auto edge_draw_array = [context, param, &edge_bdd_edge, &edge_project, &spline_split_t](
        Array_t<Pos> edge_data, // Spline data (point, control, point, control, ..., point)
        Bdd_attr bdd0, // Start node
        Bdd_attr bdd1, // End node
        float dash_length,
        float t_max, // How much of the spline to draw. (For example, t_max = 1/3 would draw only the first third.)
        u8* stroke, // The stroke color
        Pos* out_p1, // Write the position of the last control point drawn back
        Pos* out_p2 // Write the drawn end position of the spline back
    ) {
        s64 l = edge_data.size;
        assert(l % 2 == 1);

        // Ensure that the edge starts and ends at the right point
        edge_project(&edge_data, {bdd0.x, bdd0.y}, {bdd1.x, bdd1.y});

        // Cut away the start and end segments so that they start and end at node boundaries
        edge_bdd_edge(&edge_data[0  ], &edge_data[1  ], &edge_data[2  ], bdd0);
        edge_bdd_edge(&edge_data[l-1], &edge_data[l-2], &edge_data[l-3], bdd1);

        // Calculate the total length of the spline
        float length_max = INFINITY;
        if (t_max != 1.f) {
            float length = 0;
            for (s64 j = 0; j+2 < l; j += 2) {
                Pos p0 = edge_data[j  ];
                Pos p1 = edge_data[j+1];
                Pos p2 = edge_data[j+2];
                length += spline_length(p0, p1, p2);
            }
            length_max = length * t_max;
        }
        
        float length_cur = 0.f;
        Pos end_p1, end_p2;

        for (s64 j = 0; j+2 < l; j += 2) {
            Pos p0 = edge_data[j  ];
            Pos p1 = edge_data[j+1];
            Pos p2 = edge_data[j+2];
            float length_j = spline_length(p0, p1, p2);
            bool the_end_is_near = j+4 >= l; // Whether this is the last iteration

            // Check whether we are hitting the t_max limit
            if (length_cur + length_j > length_max) {
                // We are. So adjust the length of the last segment.
                float f = spline_find_offset(p0, p1, p2, length_max - length_cur);
                Pos spl[3];
                spline_split_t(spl, p0, p1, p2, f);
                length_j = length_max - length_cur;
                p1 = spl[0];
                p2 = spl[1];
                the_end_is_near = true;
            }
            if (the_end_is_near) {
                // This is the last segment. So make room for the arrow.
                end_p1 = p1;
                end_p2 = p2;
                float f = param.arrow_size*0.98 < length_j
                    ? 1.f - spline_find_offset(p2, p1, p0, param.arrow_size*0.98) : 0.f;
                Pos spl[3];
                spline_split_t(spl, p0, p1, p2, f);
                length_j -= param.arrow_size*0.98;
                p1 = spl[0];
                p2 = spl[1];
            }
            opengl_draw_edge(context, p0, p1, p2, param.edge_size, dash_length / length_j, length_cur / length_j, stroke);
            length_cur += length_j;
            if (the_end_is_near) break;
        }

        if (out_p1) { *out_p1 = end_p1; }
        if (out_p2) { *out_p2 = end_p2; }
    };
    
    for (u32 i = 2; i < 2*store.bdd_data.size; ++i) {
        float dash_length = i & 1 ? 0.f : param.dash_length;
        Bdd_attr bdd0 = attr_cur[i >> 1];
        
        // Check whether the edge is marked
        u8 stroke_col[] = {0, 0, 0, 0};
        
        u8 stroke_marked[] = {0, 0, 0, 0};
        color_set(stroke_marked, 0x884babff);
        color_inter(stroke_col, stroke_marked, i&1 ? bdd0.mark_child1 : bdd0.mark_child0);
        
        stroke_col[3] = bdd0.stroke[3];
        
        edge_data.size = 0;

        if (edge_map0[i] != -1 and edge_map1[i] != -1) {
            // Edge exists in both frames
            
            Edge edge0 = layouts[frame  ].edges[edge_map0[i]];
            Edge edge1 = layouts[frame+1].edges[edge_map1[i]];
            Array_t<Pos> edge0_data = array_subarray(layouts[frame  ].edge_data,
                edge0.offset, layouts[frame  ].edges[edge_map0[i]+1].offset);
            Array_t<Pos> edge1_data = array_subarray(layouts[frame+1].edge_data,
                edge1.offset, layouts[frame+1].edges[edge_map1[i]+1].offset);

            // Either we are pointing to the same node in both frame or we are not
            if (bdds0[edge0.to].id == bdds1[edge1.to].id) {
                // We are. So just interpolate the edge and draw it.
                Bdd_attr bdd1 = attr_cur[bdds0[edge0.to].id];
                Pos p1, p2;
                edge_mix_points(&edge_data, edge0_data, edge1_data, t);
                edge_draw_array(edge_data, bdd0, bdd1, dash_length, 1.f, stroke_col, &p1, &p2);
                opengl_draw_arrow(context, p1, p2, param.arrow_size, stroke_col);
            } else {
                // We are not. This is the same as both destroying and creating an edge at the same
                // time, so do both animations.
                
                Bdd_attr bdd10 = attr_cur[bdds0[edge0.to].id];
                Pos p1, p2;
                u8 stroke_col2[] = {stroke_col[0], stroke_col[1], stroke_col[2], stroke_col[3]};
                stroke_col2[3] = (u8)(((u64)stroke_col2[3] * (u64)bdd10.stroke[3]) / 255);
                array_append(&edge_data, edge0_data);
                edge_draw_array(edge_data, bdd0, bdd10, dash_length, 1.f, stroke_col2, &p1, &p2);
                opengl_draw_arrow(context, p1, p2, param.arrow_size, stroke_col2);

                float length;
                edge_data.size = 0;
                array_append(&edge_data, edge1_data);
                Pos p = edge_data[edge_data.size-1];
                Bdd_attr bdd11 = {p.x, p.y, param.node_radius, param.node_radius*param.squish_fac};
                edge_draw_array(edge_data, bdd0, bdd11, dash_length, t, stroke_col, &p1, &p2);
                opengl_draw_arrow(context, p1, p2, param.arrow_size, stroke_col);
            }
        } else if (edge_map0[i] == -1 and edge_map1[i] != -1) {
            // The edge was created
            Edge edge1 = layouts[frame+1].edges[edge_map1[i]];
            Array_t<Pos> edge1_data = array_subarray(layouts[frame+1].edge_data,
                edge1.offset, layouts[frame+1].edges[edge_map1[i]+1].offset);
            Bdd_attr bdd1 = attr_cur[bdds1[edge1.to].id];
            array_append(&edge_data, edge1_data);

            // Generally, we want to 'shoot' the edge at the target. However, if the child node is
            // doing the usual 'popping out' animation, we draw the full edge instead.
            Pos p1, p2;
            if (bdd1.y <= std::floor(bdd1.y)) {
                float length;
                Pos p = edge_data[edge_data.size-1];
                bdd1 = {p.x, p.y, param.node_radius, param.node_radius*param.squish_fac};
                edge_draw_array(edge_data, bdd0, bdd1, dash_length, t, stroke_col, &p1, &p2);
                opengl_draw_arrow(context, p1, p2, param.arrow_size, stroke_col);
            } else {
                // Somewhat hacky way to detect that the child is being created in the way where it
                // pops out of the parent (you know, the natural way) instead of just appearing out
                // of thin air; which happens when a child already has children at creation.
                stroke_col[3] = (u8)(((u64)stroke_col[3] * (u64)bdd1.stroke[3]) / 255);
                edge_draw_array(edge_data, bdd0, bdd1, dash_length, 1.f, stroke_col, &p1, &p2);
                opengl_draw_arrow(context, p1, p2, param.arrow_size, stroke_col);
            }
        } else if (edge_map0[i] != -1 and edge_map1[i] == -1) {
            // Edge is destroyed. Fade it out.
            Edge edge0 = layouts[frame  ].edges[edge_map0[i]];
            Array_t<Pos> edge0_data = array_subarray(layouts[frame  ].edge_data,
                edge0.offset, layouts[frame  ].edges[edge_map0[i]+1].offset);
            Bdd_attr bdd1 = attr_cur[bdds0[edge0.to].id];
            array_append(&edge_data, edge0_data);

            stroke_col[3] = std::min(stroke_col[3], bdd1.stroke[3]);

            Pos p1, p2;
            edge_draw_array(edge_data, bdd0, bdd1, dash_length, 1.f, stroke_col, &p1, &p2);
            opengl_draw_arrow(context, p1, p2, param.arrow_size, stroke_col);
        } else {
            // Edge exists in neither frame, so do nothing
        }
    }
}

namespace Ui_elem {

enum Name: u8 {
    INVALID, OP_NODE0, OP_NODE1, CREATE_TYPE, CREATE_NUMS, CREATE_FORM, CREATE_BASE,
    CREATE_BITS, CREATE_VARS, OPERATION,
    NAME_COUNT
};

char* name[] = {
    nullptr, "op_node0", "op_node1", "create_type", "create_nums", "create_form", "create_base",
    "create_bits", "create_vars", "operation", nullptr
};

}

struct Animation {
    double begin = 0.0, duration = 1.0;
    float value0 = 0.f, value1, speed0, speed1;
};

// Return the coefficients for the polynomial of the animation. a is the constant term. The
// polynomial p is the unique third-degree polynomial with
//     p (0) = anim->value0
//     p (1) = anim->speed0
//     p'(0) = anim->value1
//     p'(1) = anim->speed1
void _animation_coeff(Animation* anim, float* a, float* b, float* c, float* d) {
    *a = anim->value0;
    *b = anim->speed0;
    *c = 3.f*(anim->value1 - *a) - 2.f*(*b) - anim->speed1;
    *d = 2.f*(*a - anim->value1) + *b + anim->speed1;
}

void animation_add(Animation* anim, float add, float min, float max) {
    double now = platform_now();
    if (anim->begin == 0.0) {
        anim->begin = now;
        anim->speed0 = 0.f;
        // value0 remains
        anim->value1 = anim->value0 + add;
        anim->speed1 = 0.f;
    } else {
        float x = (float)((now - anim->begin) / anim->duration);
        
        float a, b, c, d;
        _animation_coeff(anim, &a, &b, &c, &d);
        
        anim->begin = now;
        anim->value0 = ((d*x + c)*x + b)*x + a;
        anim->speed0 = (3.f*d*x + 2.f*c)*x + b;
        anim->value1 += add;
        anim->speed1 = 0.f;
    }

    anim->value1 = std::min(anim->value1, max);
    anim->value1 = std::max(anim->value1, min);
}

void animation_set(Animation* anim, float value, float min = -INFINITY, float max = INFINITY) {
    anim->begin = 0.f;
    anim->value0 = value;
    anim->value0 = std::min(anim->value0, max);
    anim->value0 = std::max(anim->value0, min);
    anim->value1 = anim->value0;
}

float animation_get(Animation* anim, bool* need_redraw = nullptr) {
    if (anim->begin == 0.f) {
        if (need_redraw) *need_redraw = false;
        return anim->value0;
    }
    
    float x = (float)((platform_now() - anim->begin) / anim->duration);
    
    if (need_redraw) *need_redraw = x < 1.f;
    if (x >= 1.f) {
        x = 1.f;
        anim->begin = 0.f;
        anim->value0 = anim->value1;
        return anim->value0;
    }

    float a, b, c, d;
    _animation_coeff(anim, &a, &b, &c, &d);
    return ((d*x + c)*x + b)*x + a;
}

// Data for the UI
struct Ui_context {
    Array_dyn<u8> ui_buf;
    Array_dyn<u32> buf_id_map;
    Array_dyn<u32> buf_id_map_end;
    Array_dyn<u32> buf_children;
    
    Array_dyn<s64> frame_section; // Stores the frames between algorithms

    // Time and frame control for the smooth animation
    Animation anim_frame;
    float frame_cur; // Caches the value of the current frame

    // Data for showing the debug information
    bool debug_info_enabled;
    constexpr static s64 time_diff_num = 128;
    float time_diff[time_diff_num] = {};
    s64 time_diff_index = 0;

    u64 focus_flags; // Bitset of the input elements that are focused

    bool is_helptext_visible = false; // You can probably guess what this flag is referring to.
    u8 novice_create_helper = 0; // "Create and Add doubleclick assistant" status
};

Ui_context global_ui;

// Update the context display, i.e. the step-by-step feature
void ui_context_refresh() {
#ifdef OBST_DBG_SHOW_FORCE_LAYOUT
    // Context could be out of bounds here, is not useful anyway
    return;
#endif

    if (not global_context.not_completely_empty) return;
    
    s64 frame = (s64)global_ui.anim_frame.value1;
    if (frame + 1 >= global_store.snapshots.size) {
        frame = global_store.snapshots.size - 2;
    }
    
    platform_fmt_init();
    platform_fmt_begin(Text_fmt::INDENTED);
    s64 last = global_store.snapshots[frame].offset_context;
    auto sd = global_store.snapshot_data_context;
    
    platform_fmt_begin(Text_fmt::PARAGRAPH_CLOSE);
    for (s64 i = global_store.snapshots[frame].offset_context;
         i < global_store.snapshots[frame+1].offset_context; ++i)
    {
        bool flush = false;
        if (strncmp((char*)&sd[i], "<b>", 3) == 0)  flush = true;
        if (strncmp((char*)&sd[i], "<s>", 3) == 0)  flush = true;
        if (strncmp((char*)&sd[i], "<i>", 3) == 0)  flush = true;
        if (strncmp((char*)&sd[i], "</b>", 4) == 0) flush = true;
        if (strncmp((char*)&sd[i], "</s>", 4) == 0) flush = true;
        if (strncmp((char*)&sd[i], "</i>", 4) == 0) flush = true;
        if (sd[i] == 0) flush = true;

        if (not flush) continue;

        u64 nospace = last < i and sd[i-1] == ' ' ? 0 : (u64)Text_fmt::NOSPACE;
        platform_fmt_text(nospace, array_subarray(sd, last, i));
            
        if (strncmp((char*)&sd[i], "<b>", 3) == 0)  { i += 2; platform_fmt_begin(Text_fmt::BOLD); }
        if (strncmp((char*)&sd[i], "<s>", 3) == 0)  { i += 2; platform_fmt_begin(Text_fmt::SANS); }
        if (strncmp((char*)&sd[i], "<i>", 3) == 0)  { i += 2; platform_fmt_begin(Text_fmt::ITALICS); }
        if (strncmp((char*)&sd[i], "</b>", 4) == 0) { i += 3; platform_fmt_end(Text_fmt::BOLD); }
        if (strncmp((char*)&sd[i], "</s>", 4) == 0) { i += 3; platform_fmt_end(Text_fmt::SANS); }
        if (strncmp((char*)&sd[i], "</i>", 4) == 0) { i += 3; platform_fmt_end(Text_fmt::ITALICS); }
        if (sd[i] == 0 and i+1 < global_store.snapshots[frame+1].offset_context) {
            platform_fmt_end(Text_fmt::PARAGRAPH_CLOSE);
            platform_fmt_begin(Text_fmt::PARAGRAPH_CLOSE);
        }

        last = i+1;
    }
    platform_fmt_end(Text_fmt::PARAGRAPH_CLOSE);
    platform_fmt_store(Text_fmt::SLOT_CONTEXT);

    global_ui.ui_buf.size = 0;
    array_printf(&global_ui.ui_buf, "%lld/%lld", frame, global_layouts.size-1);
    platform_fmt_store_simple(Text_fmt::NOSPACE, global_ui.ui_buf, Text_fmt::SLOT_CONTEXT_FRAME);
}

// Get all numbers stored by a node
void _collect_children(Array_dyn<u32>* children, Array_dyn<u32> id_map, Array_t<Bdd> bdds, u32 bdd) {
    if (bdd == 1) {
        // This is the empty bitstring
        array_push_back(children, (u32)0);
    } else if (bdd == 0) {
        // nothing
    } else {
        Bdd i = bdds[id_map[bdd]];
        {Bdd child0;
        if (id_map[i.child0] != (u32)-1) {
            child0 = bdds[id_map[i.child0]];
        } else {
            child0 = global_store.bdd_data[i.child0];
        }

        s64 index = children->size;

        _collect_children(children, id_map, bdds, i.child0);
        
        // Add any levels the edges skips back in
        for (u32 level = child0.level+1; level < i.level; ++level) {
            s64 size = children->size;
            for (s64 i = index; i < size; ++i) {
                array_push_back(children, ((u32)1 << (level-1)) | (*children)[i]);
            }
        }}

        // Same for child1
        {Bdd child1;
        if (id_map[i.child1] != (u32)-1) {
            child1 = bdds[id_map[i.child1]];
        } else {
            child1 = global_store.bdd_data[i.child1];
        }

        s64 index = children->size;

        _collect_children(children, id_map, bdds, i.child1);
        
        for (u32 level = child1.level+1; level < i.level; ++level) {
            s64 size = children->size;
            for (s64 i = index; i < size; ++i) {
                array_push_back(children, ((u32)1 << (level-1)) | (*children)[i]);
            }
        }
        for (s64 j = index; j < children->size; ++j) {
            (*children)[j] |= ((u32)1 << (i.level-1));
        }}
    }
};

// Display the hover text. Returns whether the bdd is currently valid.
bool ui_bddinfo_show(float x, float y, u32 bdd) {
    // Depending on whether we are moving forwards or backwards, round to the next frame
    s64 frame = global_ui.frame_cur < global_ui.anim_frame.value1
        ? (s64)std::floor(global_ui.frame_cur) : (s64)std::ceil(global_ui.frame_cur);
    
    if (frame and frame == global_store.snapshots.size-1) {
        // There _could_ be some rounding errors in displaying the last frame.
        --frame;
    } else if (frame > global_store.snapshots.size-1) {
        // This just does not make any sense. Something's wrong, silently abort.
        return false;
    }

    array_resize(&global_ui.buf_id_map, global_store.bdd_data.size);
    Array_dyn<u32> id_map = global_ui.buf_id_map;
    memset(id_map.data, -1, id_map.size * sizeof(id_map[0]));
    
    array_resize(&global_ui.buf_id_map_end, global_store.bdd_data.size);
    Array_dyn<u32> id_map_end = global_ui.buf_id_map_end;
    memset(id_map_end.data, -1, id_map_end.size * sizeof(id_map_end[0]));

    auto bdds = array_subarray(
        global_store.snapshot_data_bdd,
        global_store.snapshots[frame].offset_bdd,
        global_store.snapshots[frame+1].offset_bdd
    );
    
    auto bdds_end = array_subarray(
        global_store.snapshot_data_bdd,
        global_store.snapshots[global_store.snapshots.size-2].offset_bdd,
        global_store.snapshots[global_store.snapshots.size-1].offset_bdd
    );

    for (s64 i = 0; i < bdds.size; ++i) {
        id_map[bdds[i].id] = i;
    }
    for (s64 i = 0; i < bdds_end.size; ++i) {
        id_map_end[bdds_end[i].id] = i;
    }

    if (id_map[bdd] == (u32)-1) {
        return false;
    }

    Array_dyn<u32> children = global_ui.buf_children;
    defer { global_ui.buf_children = children; };
    children.size = 0;

    _collect_children(&children, id_map, bdds, bdd);
    
    Bdd bdd_bdd = bdds[id_map[bdd]];
    
    Array_dyn<u8> buf = global_ui.ui_buf;
    defer { global_ui.ui_buf = buf; };

    platform_fmt_init();

    auto print_children = [&]() {
        bool first = true;
        for (u32 child: children) {
            if (not first) {
                platform_fmt_text(0, ",");
            }
            first = false;

            for (s64 i = bdd_bdd.level-1; i >= 0; --i) {
                platform_fmt_text(Text_fmt::STICKY, child >> i & 1 ? "1" : "0");
            }
        }
        platform_fmt_end(Text_fmt::PARAGRAPH_CLOSE);
        platform_fmt_text(0, "(Decimal:");
        first = true;
        for (u32 child: children) {
            if (not first) {
                platform_fmt_text(0, ",");
            }
            first = false;
            buf.size = 0;
            array_printf(&buf, "%d", child);
            platform_fmt_text(Text_fmt::STICKY, buf);
        }
        platform_fmt_text(0, ")");
    };
    
    // Generate the info text
    if (bdd == 1) {
        platform_fmt_text(Text_fmt::BOLD | Text_fmt::PARAGRAPH_CLOSE, "Node T");
        platform_fmt_text(Text_fmt::PARAGRAPH, "This node is special. It represents "
            "the set containing only the number 0 (or, more precisely, the empty bitstring). Its "
            "sibling, F, is the empty set and is omitted from the drawing.");
    } else {
        buf.size = 0;
        platform_fmt_text(Text_fmt::BOLD, "Node");
        auto name = array_subarray(global_store.name_data, global_store.names[global_store.bdd_data[bdd].name],
            global_store.names[global_store.bdd_data[bdd].name+1]);
        for (u8 c: name) {
            auto cc = opengl_bddlabel_index_utf8(opengl_bddlabel_char_index(c));
            platform_fmt_text(Text_fmt::BOLD | Text_fmt::NOSPACE, cc);
        }
        if (bdd_bdd.flags & Bdd::TEMPORARY) {
            platform_fmt_text(Text_fmt::BOLD, " (temporary)");
        }
        platform_fmt_end(Text_fmt::PARAGRAPH_CLOSE);
        
        if (children.size) {
            buf.size = 0;
            array_printf(&buf, "%s number%s ",
                bdd_bdd.flags & Bdd::TEMPORARY ? "Currently represents" : "Represents",
                children.size > 1 ? "s" : "");
            platform_fmt_text(0, buf);
            print_children();
        } else {
            if (bdd_bdd.flags & Bdd::TEMPORARY) {
                platform_fmt_text(0, "Does not represent any numbers (yet)");
            } else {
                platform_fmt_text(0, "Empty set");
            }
        }
        if (bdd_bdd.flags & Bdd::TEMPORARY and id_map_end[bdd] != -1) {
            // The node is still there in the end, so tell the user what it will contain later
            
            children.size = 0;
            _collect_children(&children, id_map_end, bdds_end, bdd);
     
            platform_fmt_end(Text_fmt::PARAGRAPH_CLOSE);
            buf.size = 0;
            array_printf(&buf, "Eventually, it will represent number%s ", children.size != 1 ? "s" : "");
            platform_fmt_text(0, buf);
            print_children();
        }

    }
    platform_fmt_end(Text_fmt::PARAGRAPH_CLOSE);
    platform_fmt_store(Text_fmt::SLOT_BDDINFO);

    float pad = global_context.draw_param.node_radius * 1.35;
    platform_ui_bddinfo_show(x, y, pad);
    
    return true;
}

// Deal with mouse motion events. Shows and hides the bddinfo hover text.
//  IMPORTANT: The arguments are NOT in pixels, but in world coordinates. So this is only useful for
// canvas stuff.
void ui_mouse_move(float world_x, float world_y) {
    if (platform_ui_help_active()) {
        platform_ui_bddinfo_hide();
        return;
    }
    
    for (s64 i = 0; i < global_context.buf_attr_cur.size; ++i) {
        Bdd_attr bdd = global_context.buf_attr_cur[i];
        float dx = (world_x - bdd.x) / bdd.rx;
        float dy = (world_y - bdd.y) / bdd.ry;
        float d = dx * dx + dy * dy;

        if (d <= 1.f) {
            // Important: Do not break the loop if the bdd was not valid and no hover text was generated.
            if (ui_bddinfo_show(bdd.x, bdd.y, i)) return;
        }
    }
    platform_ui_bddinfo_hide();
}

void ui_frame_draw() {
    float then = (float)platform_now(); //@Cleanup: Move times to double
    opengl_frame_init(&global_context);
    layout_frame_draw(&global_context, global_layouts, global_store, global_ui.frame_cur);

    // We need the font to draw text, so check that not_completely_empty flag
    if (global_ui.debug_info_enabled and global_context.not_completely_empty) {
        // Draw the debug information
        float last = global_ui.time_diff[global_ui.time_diff_index];
        float max = 0.f;
        float avg = 0.f;
        for (s64 i = 0; i < global_ui.time_diff_num; ++i) {
            if (max < global_ui.time_diff[i]) max = global_ui.time_diff[i];
            avg += global_ui.time_diff[i];
        }
        avg /= global_ui.time_diff_num;
        
        {u8 buf[32];
        float x = -0.6f;
        float y = global_context.layout_max_y + 0.9f;
        float fs = 12.f / global_context.scale;
        snprintf((char*)buf, sizeof(buf), "%.0f", last * 1e4);
        opengl_draw_text(&global_context, x, y, {buf, (s64)strlen((char*)buf)}, fs, 1.f, 1.f);
        y -= fs;
        snprintf((char*)buf, sizeof(buf), "%.0f", max * 1e4);
        opengl_draw_text(&global_context, x, y, {buf, (s64)strlen((char*)buf)}, fs, 1.f, 1.f);
        y -= fs;
        snprintf((char*)buf, sizeof(buf), "%.0f", avg * 1e4);
        opengl_draw_text(&global_context, x, y, {buf, (s64)strlen((char*)buf)}, fs, 1.f, 1.f);}

        float x = -0.6f + 5.f / global_context.scale;
        float y = global_context.layout_max_y + 0.9f - 30.f / global_context.scale;
        float yfac = 1000.f / global_context.scale;
        for (s64 i = 0; i+1 < global_ui.time_diff_num; ++i) {
            opengl_draw_edge_simple(
                &global_context,
                {x, y + global_ui.time_diff[i]*yfac},
                {x, y + global_ui.time_diff[i+1]*yfac},
                1.f / global_context.scale, 0x111111ff
            );
            x += 1.f / global_context.scale;
        }
    }
    
    opengl_frame_draw(&global_context);

    // Pretend we moved the mouse to update the bddinfo hover text
    {float x, y;
    platform_mouse_position(&x, &y);
    ui_mouse_move(x, y);}

    // Register the time this took so that we can draw it for the debugging code.
    {float now = (float)platform_now();
    global_ui.time_diff_index = (global_ui.time_diff_index + 1) % global_ui.time_diff_num;
    global_ui.time_diff[global_ui.time_diff_index] = now - then;}
}

void ui_set_frame(s64 frame) {
    if (frame <= 0) {
        frame = 0;
    } else if (frame >= global_layouts.size) {
        frame = global_layouts.size-1;
    }
    animation_set(&global_ui.anim_frame, (float)frame);
    ui_context_refresh();
    platform_main_loop_active(true);
}

// After updating the store, this re-renders the layouts and shows the results in the UI.
void ui_commit_store() {
    layout_render(&global_layouts, &global_context.layout_max_x, &global_context.layout_max_y,
        &global_context.layout_max_points, global_store);
    global_context.font_regenerate = 1;
    global_context.not_completely_empty = true;

    platform_fmt_store_simple(0, "", Text_fmt::SLOT_ERRORINFO);
    
    array_push_back(&global_ui.frame_section, global_layouts.size);
    float frame = global_ui.frame_section[global_ui.frame_section.size-2];
    ui_set_frame(frame);
}

// Called when the user presses the button responsible for union, intersection and negation.
void ui_button_op() {
    if (not global_context.not_completely_empty) return;
    
    global_ui.novice_create_helper = 2; // Deactivate the "Create and Add doubleclick assistant"

    Array_t<u8> op_str   = platform_ui_value_get(Ui_elem::OPERATION);
    Array_t<u8> arg0_str = platform_ui_value_get(Ui_elem::OP_NODE0);
    Array_t<u8> arg1_str = platform_ui_value_get(Ui_elem::OP_NODE1);

    defer { platform_ui_value_free(op_str); };
    defer { platform_ui_value_free(arg0_str); };
    defer { platform_ui_value_free(arg1_str); };
    
    assert(op_str.size == 1);

    auto parse_bdd = [](s32* arg, Array_t<u8> arg_str, char const* desc) {
        if (arg_str.size == 1 and arg_str[0] == 'F') {
            *arg = 0;
        } else if (arg_str.size == 1 and arg_str[0] == 'T') {
            *arg = 1;
        } else if (u8 code = jup_stoi(arg_str, arg, 10)) {
            ui_error_report("Error while parsing '%s', which has to be a number: %s", desc, jup_err_messages[code]);
            return true;
        } else if (*arg < 0) {
            ui_error_report("Error: '%s' must be nonnegative.", desc);
            return true;
        } else if (*arg >= global_store.bdd_data.size or global_store.bdd_data[*arg].flags & Bdd::TEMPORARY) {
            ui_error_report("Error: '%s' is not a valid node.", desc);
            return true;
        }
        return false;
    };

    if (op_str[0] == 'u') {
        s32 arg0, arg1;
        if (parse_bdd(&arg0, arg0_str, "First node")) return;
        if (parse_bdd(&arg1, arg1_str, "Second node")) return;
        bdd_union_stepwise(&global_store, arg0, arg1);
    } else if (op_str[0] == 'i') {
        s32 arg0, arg1;
        if (parse_bdd(&arg0, arg0_str, "First node")) return;
        if (parse_bdd(&arg1, arg1_str, "Second node")) return;
        bdd_intersection_stepwise(&global_store, arg0, arg1);
    } else if (op_str[0] == 'c') {
        s32 arg0;
        if (parse_bdd(&arg0, arg0_str, "First node")) return;
        bdd_complement_stepwise(&global_store, arg0);
    } else {
        assert_false;
    }

    ui_commit_store();
}

bool ui_button_create_from_numbers() {
    Array_t<u8> nums_str = platform_ui_value_get(Ui_elem::CREATE_NUMS);
    Array_t<u8> base_str = platform_ui_value_get(Ui_elem::CREATE_BASE);
    Array_t<u8> bits_str = platform_ui_value_get(Ui_elem::CREATE_BITS);

    defer { platform_ui_value_free(nums_str); };
    defer { platform_ui_value_free(base_str); };
    defer { platform_ui_value_free(bits_str); };

    s32 base;
    if (u8 code = jup_stoi(base_str, &base, 10)) {
        ui_error_report("Error while parsing 'Base', which has to be a number: %s", jup_err_messages[code]);
        return false;
    } else if (base < 2 or 36 < base) {
        ui_error_report("Error: 'Base' must be between 2 and 36. Tryhard.");
        return false;
    }
    
    Array_dyn<s64> nums;
    if (u8 code = parse_int_list(&nums, nums_str, base)) {
        ui_error_report("Error while parsing numbers, which has to be a comma-delimited list of numbers. Error: %s", jup_err_messages[code]);
        return false;
    }
    for (s64 i: nums) {
        if (i < 0) {
            ui_error_report("Error: Only non-negative numbers are valid. (What were you trying to achieve, anyway?)");
            return false;    
        }
    }

    Array_dyn<s64> bits;
    if (strncmp((char*)bits_str.data, "auto", bits_str.size) == 0) {
        s64 bit_max = 1;
        for (s64 i: nums) {
            s64 bit_cur = 64 - __builtin_clzll(i | 1);
            if (bit_max < bit_cur) bit_max = bit_cur;
        }
        for (s64 i = bit_max-1; i >= 0 ; --i) {
            array_push_back(&bits, i);
        }
    } else if (u8 code = parse_int_list(&bits, bits_str, base)) {
        ui_error_report("Error while parsing 'Bit order': %s", jup_err_messages[code]);
        return false;
    } else if (bits.size < 1) {
        ui_error_report("Error: 'Bit order' must contain at least one element.");
        return false;
    }
    Array_dyn<u8> bits_u8;
    for (s64 i: bits) {
        if (i < 0 or i > 30) {
            ui_error_report("Error: Invalid bit index, must be between 0 and 30.");
            return false;
        }
        array_push_back(&bits_u8, (u8)i);
    }

    // This performs the actual algorithm
    u32 bdd = bdd_from_list_stepwise(&global_store, {(u64*)nums.data, nums.size}, bits_u8, base);

    platform_operations_enable(bdd);
    ui_commit_store();
    return true;
}

bool ui_button_create_from_formula() {
    Array_t<u8> form_str = platform_ui_value_get(Ui_elem::CREATE_FORM);
    Array_t<u8> vars_str = platform_ui_value_get(Ui_elem::CREATE_VARS);
    defer { platform_ui_value_free(form_str); };
    defer { platform_ui_value_free(vars_str); };

    auto fstore = &global_formula_store;
    
    formula_store_init(fstore);
    
    s64 f_id = formula_parse(fstore, form_str, vars_str);
    if (f_id == -1) {
        s64 elem = fstore->str.data == form_str.data ? Ui_elem::CREATE_FORM : Ui_elem::CREATE_VARS;
        platform_ui_cursor_set(elem, fstore->str_i, fstore->str_row, fstore->str_col, fstore->str_char);
        return false;
    }

    u32 bdd = bdd_from_formula_stepwise(&global_store, fstore, f_id);
    
    platform_operations_enable(bdd);
    ui_commit_store();
    return true;
}

// Callback for the 'Create and add' button
void ui_button_create() {
    // "Create and Add doubleclick assistant": If someone presses the "Create and add" button twice,
    // they probably did not mean to. So, we help them by moving the animation towards the end
    // instead.
    if (global_ui.novice_create_helper == 0) {
        global_ui.novice_create_helper = 1;
    } else if (global_ui.novice_create_helper == 1) {
        global_ui.novice_create_helper = 2;
        global_ui.anim_frame.duration = (float)global_layouts.size * 0.2f;
        animation_add(&global_ui.anim_frame, INFINITY, 0.f, (float)std::max(global_layouts.size-1, 0ll));
        global_ui.anim_frame.speed0 = (global_ui.anim_frame.value1 - global_ui.anim_frame.value0);
        global_ui.anim_frame.speed1 = global_ui.anim_frame.speed0;
        ui_context_refresh();
        platform_main_loop_active(true);
        return;
    }
    
    Array_t<u8> type_str = platform_ui_value_get(Ui_elem::CREATE_TYPE);
    defer { platform_ui_value_free(type_str); };

    assert(type_str.size == 1);

    bool result;
    if (type_str[0] == 'n') {
        result = ui_button_create_from_numbers();
    } else {
        result = ui_button_create_from_formula();
    }

    if (not result and global_ui.novice_create_helper == 1) {
        global_ui.novice_create_helper = 0;
    }
}

// Called when the 'Remove all' button is pressed. Also used to initialise the UI.
void ui_button_removeall() {
    global_ui.novice_create_helper = 2; // Deactivate the "Create and Add doubleclick assistant"

    bdd_store_init(&global_store);
    global_layouts.size = 0;

    global_context.not_completely_empty = false;

    global_ui.frame_section.size = 0;
    array_push_back(&global_ui.frame_section, 0ll);
    animation_set(&global_ui.anim_frame, 0.f);

    platform_operations_disable();

    ui_frame_draw();
    platform_fmt_store_simple(0, "", Text_fmt::SLOT_CONTEXT);
    platform_fmt_store_simple(Text_fmt::NOSPACE, "0/0", Text_fmt::SLOT_CONTEXT_FRAME);
    platform_fmt_store_simple(0, "", Text_fmt::SLOT_ERRORINFO);
}

// This is called whenever the canvas/window resizes. The new dimensions are already applied in
// global_context.
void application_handle_resize() {
    // Font regeneration scheduled in 10 frames. I do not want to do this every frame while the user
    // changes window size.
    global_context.font_regenerate = 10;
    platform_main_loop_active(true);
}

void application_render() {
    bool redraw;
    global_ui.frame_cur = animation_get(&global_ui.anim_frame, &redraw);

    // Check whether the animation has stopped
    if (not redraw) {
        // If the font is due to regenerate, i.e. we are resizing, we actually do want to draw, so
        // check that. Else, we will come back to this.
        if (global_context.font_regenerate == 0) {
            platform_main_loop_active(false);
        }
    }

    ui_frame_draw();
}

// Moving in animation frames
void ui_button_move(float diff) {
    global_ui.novice_create_helper = 2; // Deactivate the "Create and Add doubleclick assistant"
    
    global_ui.anim_frame.duration = 0.5f;
    animation_add(&global_ui.anim_frame, diff, 0.f, (float)std::max(global_layouts.size-1, 0ll));
    ui_context_refresh();
    platform_main_loop_active(true);
}

// Used to jump to the next/last point where an algorithm started.
void ui_page(s64 diff) {
    s64 frame_target;
    if (diff > 0) {
        frame_target = global_ui.frame_section[global_ui.frame_section.size - 1];
        for (s64 i = 0; i < global_ui.frame_section.size; ++i) {
            if (global_ui.frame_section[i] > global_ui.frame_cur) {
                frame_target = global_ui.frame_section[i];
                break;
            }
        }
    } else if (diff < 0) {
        frame_target = global_ui.frame_section[0];
        for (s64 i = global_ui.frame_section.size-1; i >= 0; --i) {
            if (global_ui.frame_section[i] < global_ui.frame_cur) {
                frame_target = global_ui.frame_section[i];
                break;
            }
        }
    } else {
        assert_false;
    }
    if (frame_target == global_layouts.size and frame_target) --frame_target;

    ui_set_frame(frame_target);
}

// Handle a single keypress. Return whether the event was consumed.
bool ui_key_press(Key key) {
    if (key.type != Key::SPECIAL) return false;

    if (key.special == Key::ARROW_R) {
        ui_button_move(1.f);
    } else if (key.special == Key::ARROW_L) {
        ui_button_move(-1.f);
        
    } else if (key.special == Key::PAGE_U) {
        ui_page(1);
    } else if (key.special == Key::PAGE_D) {
        ui_page(-1);
        
    } else if (key.special == Key::HOME) {
        ui_set_frame(0);
    } else if (key.special == Key::END) {
        ui_set_frame(0x7fffffffffffffffull);
        
    } else if (key.special == Key::F1) {
        platform_ui_button_help();
    } else if (key.special == Key::F2) {
        global_ui.debug_info_enabled = not global_ui.debug_info_enabled;
    } else if (key.special == Key::F3) {
        platform_panel_toggle();
        
    } else {
        return false;
    }
    return true;
}

// This is the entry point for platform-independent initialisation.
void application_init() {
    bdd_store_init(&global_store);
    ui_button_removeall();
    
    global_ui.novice_create_helper = 0; // Activate the "Create and Add doubleclick assistant"
}
