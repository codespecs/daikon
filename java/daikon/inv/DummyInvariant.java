package daikon.inv;

import daikon.PptSlice;
import daikon.PptSlice1;
import daikon.PptSlice2;
import daikon.PptSlice3;
import daikon.PptTopLevel;
import daikon.VarInfo;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.NonPrototype;
import typequals.prototype.qual.Prototype;

/**
 * This is a special invariant used internally by Daikon to represent invariants whose meaning
 * Daikon doesn't understand. The only operation that can be performed on a DummyInvariant is to
 * print it. In particular, the invariant cannot be tested against a sample: the invariant is always
 * assumed to hold and is always considered to be statistically justified.
 *
 * <p>The main use for a dummy invariant is to represent a splitting condition that appears in a
 * {@code .spinfo} file. The {@code .spinfo} file can indicate an arbitrary Java expression, which
 * might not be equivalent to any invariant in Daikon's grammar.
 *
 * <p>Ordinarily, Daikon uses splitting conditions to split data, then seeks to use that split data
 * to form conditional invariants out of its standard built-in invariants. If you wish the
 * expression in the .spinfo file to be printed as an invariant, whether or not it is itself
 * discovered by Daikon during invariant detection, then the configuration option {@code
 * daikon.split.PptSplitter.dummy_invariant_level} must be set, and formatting information must be
 * supplied in the splitter info file.
 */
public class DummyInvariant extends Invariant {
  static final long serialVersionUID = 20030220L;

  private @Nullable String daikonFormat;
  private @Nullable String javaFormat;
  private @Nullable String escFormat;
  private @Nullable String simplifyFormat;
  private @Nullable String jmlFormat;
  private @Nullable String dbcFormat;
  private @Nullable String csharpFormat;

  private boolean negated = false;

  // Pre-instatiate(), set to true if we have reason to believe the user
  // explicitly wanted this invariant to appear in the output.
  // [What evidence is required, and when does the evidence show the user
  // didn't want it?  Does the fact that this is a DummyInvariant indicate
  // the user explicitly cares?]
  // After instantiation, also requires that we've found an appropriate
  // slice for the invariant to live in.
  public boolean valid = false;

  public DummyInvariant(
      PptSlice ppt,
      @Nullable String daikonStr,
      @Nullable String java,
      @Nullable String esc,
      @Nullable String simplify,
      @Nullable String jml,
      @Nullable String dbc,
      @Nullable String csharp,
      boolean valid) {
    super(ppt);
    daikonFormat = daikonStr;
    javaFormat = java;
    escFormat = esc;
    simplifyFormat = simplify;
    jmlFormat = jml;
    dbcFormat = dbc;
    csharpFormat = csharp;
    this.valid = valid;
  }

  public @Prototype DummyInvariant(
      @Nullable String daikonStr,
      @Nullable String java,
      @Nullable String esc,
      @Nullable String simplify,
      @Nullable String jml,
      @Nullable String dbc,
      @Nullable String csharp,
      boolean valid) {
    super();
    daikonFormat = daikonStr;
    javaFormat = java;
    escFormat = esc;
    simplifyFormat = simplify;
    jmlFormat = jml;
    dbcFormat = dbc;
    csharpFormat = csharp;
    this.valid = valid;
  }

  public DummyInvariant instantiate(PptTopLevel parent, VarInfo[] vars) {
    assert !this.negated : "Only instantiated invariants should be negated";
    DummyInvariant inv =
        new DummyInvariant(
            ppt,
            daikonFormat,
            javaFormat,
            escFormat,
            simplifyFormat,
            jmlFormat,
            dbcFormat,
            csharpFormat,
            // Not valid until we find a slice for it
            /* valid= */ false);

    // Find between 1 and 3 unique variables, to pick a slice to put
    // this in.
    HashSet<VarInfo> uniqVarsSet = new HashSet<>();
    for (int i = 0; i < vars.length; i++) {
      uniqVarsSet.add(vars[i].canonicalRep());
    }
    int sliceSize = uniqVarsSet.size();
    if (sliceSize > 3) {
      sliceSize = 3;
    }
    /*NNC:@MonotonicNonNull*/ VarInfo[] newVars = new VarInfo[sliceSize];
    {
      Iterator<VarInfo> it = uniqVarsSet.iterator();
      int i = 0;
      while (it.hasNext()) {
        newVars[i++] = it.next();
        if (i == sliceSize) {
          break;
        }
      }
    }
    vars = newVars;
    assert vars.length >= 1 && vars.length <= 3;
    if (vars.length == 1) {
      PptSlice1 slice = parent.findSlice(vars[0]);
      if (slice == null) {
        slice = new PptSlice1(parent, vars);
        parent.addSlice(slice);
      }
      inv.ppt = slice;
    } else if (vars.length == 2) {
      if (vars[0] == vars[1]) {
        return inv;
      } else if (vars[0].varinfo_index > vars[1].varinfo_index) {
        VarInfo tmp = vars[0];
        vars[0] = vars[1];
        vars[1] = tmp;
      }
      PptSlice2 slice = parent.findSlice(vars[0], vars[1]);
      if (slice == null) {
        slice = new PptSlice2(parent, vars);
        parent.addSlice(slice);
      }
      inv.ppt = slice;
    } else if (vars.length == 3) {
      if (vars[0] == vars[1] || vars[1] == vars[2] || vars[0] == vars[2]) {
        return inv;
      }
      // bubble sort
      VarInfo tmp;
      if (vars[0].varinfo_index > vars[1].varinfo_index) {
        tmp = vars[0];
        vars[0] = vars[1];
        vars[1] = tmp;
      }
      if (vars[1].varinfo_index > vars[2].varinfo_index) {
        tmp = vars[1];
        vars[1] = vars[2];
        vars[2] = tmp;
      }
      if (vars[0].varinfo_index > vars[1].varinfo_index) {
        tmp = vars[0];
        vars[0] = vars[1];
        vars[1] = tmp;
      }
      PptSlice3 slice = parent.findSlice(vars[0], vars[1], vars[2]);
      if (slice == null) {
        slice = new PptSlice3(parent, vars);
        parent.addSlice(slice);
      }
      inv.ppt = slice;
    }
    // We found a slice, so set the DummyInvariant to valid.
    inv.valid = true;
    return inv;
  }

  @Override
  protected double computeConfidence() {
    return Invariant.CONFIDENCE_JUSTIFIED;
  }

  public void negate() {
    negated = !negated;
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied DummyInvariant this, OutputFormat format) {
    if (format == OutputFormat.DAIKON) {
      return format_daikon();
    }
    if (format == OutputFormat.JAVA) {
      return format_java();
    }
    if (format == OutputFormat.ESCJAVA) {
      return format_esc();
    }
    if (format == OutputFormat.SIMPLIFY) {
      return format_simplify();
    }
    if (format == OutputFormat.JML) {
      return format_jml();
    }
    if (format == OutputFormat.DBCJAVA) {
      return format_dbc();
    }
    if (format == OutputFormat.CSHARPCONTRACT) {
      return format_csharp();
    }

    return format_unimplemented(format);
  }

  public String format_daikon(@GuardSatisfied DummyInvariant this) {
    String df;
    if (daikonFormat == null) {
      df = "<dummy>";
    } else {
      df = daikonFormat;
    }
    if (negated) {
      return "not(" + df + ")";
    } else {
      return df;
    }
  }

  public String format_java(@GuardSatisfied DummyInvariant this) {
    if (javaFormat == null) {
      return "format_java not implemented for dummy invariant";
    }
    if (negated) {
      return "!(" + javaFormat + ")";
    } else {
      return javaFormat;
    }
  }

  public String format_esc(@GuardSatisfied DummyInvariant this) {
    if (escFormat == null) {
      return "format_esc not implemented for dummy invariant";
    }
    if (negated) {
      return "!(" + escFormat + ")";
    } else {
      return escFormat;
    }
  }

  public String format_simplify(@GuardSatisfied DummyInvariant this) {
    if (simplifyFormat == null) {
      return "format_simplify not implemented for dummy invariant";
    }
    if (negated) {
      return "(NOT " + simplifyFormat + ")";
    } else {
      return simplifyFormat;
    }
  }

  public String format_jml(@GuardSatisfied DummyInvariant this) {
    if (jmlFormat == null) {
      return "format_jml not implemented for dummy invariant";
    }
    if (negated) {
      return "!(" + jmlFormat + ")";
    } else {
      return jmlFormat;
    }
  }

  public String format_dbc(@GuardSatisfied DummyInvariant this) {
    if (dbcFormat == null) {
      return "format_dbc not implemented for dummy invariant";
    }
    if (negated) {
      return "!(" + dbcFormat + ")";
    } else {
      return dbcFormat;
    }
  }

  public String format_csharp(@GuardSatisfied DummyInvariant this) {
    if (csharpFormat == null) {
      return "format_csharp not implemented for dummy invariant";
    }
    if (negated) {
      return "!(" + csharpFormat + ")";
    } else {
      return csharpFormat;
    }
  }

  @Override
  protected Invariant resurrect_done(int[] permutation) {
    throw new Error("Not implemented");
  }

  @Override
  public boolean isSameFormula(Invariant other) {
    throw new Error("Not implemented");
  }

  @Override
  public boolean enabled(@Prototype DummyInvariant this) {
    throw new Error("do not invoke " + getClass() + ".enabled()");
  }

  @Override
  public boolean valid_types(@Prototype DummyInvariant this, VarInfo[] vis) {
    throw new Error("do not invoke " + getClass() + ".valid_types()");
  }

  @Override
  protected @NonPrototype DummyInvariant instantiate_dyn(
      @Prototype DummyInvariant this, PptSlice slice) {
    throw new Error("do not invoke " + getClass() + ".instantiate_dyn()");
  }

  @Override
  public @Nullable @NonPrototype DummyInvariant merge(
      @Prototype DummyInvariant this, List<@NonPrototype Invariant> invs, PptSlice parent_ppt) {
    throw new Error("Don't merge DummyInvariant invariants");
  }
}
