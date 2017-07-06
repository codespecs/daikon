package daikon.mints;

import daikon.PptMap;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * (M)ining Interesting Likely (In)varian(ts).
 *
 * @author Huascar A. Sanchez
 **/
class Mints {

  private final static String OUTPUT_DIRECTORY = "/Users/hsanchez/dev/vesperin/benchtop/output/";

  /** get patterns **/
  public static void main(String... args) throws IOException {

    final Path output = Paths.get(OUTPUT_DIRECTORY);

    final List<InvariantSequence> sequences = new Mints().sequenceList(output).stream().limit(10).collect(Collectors.toList());

    final Json file     = Json.object();
    final Json sources  = Json.array();

    for(InvariantSequence eachSequence : sequences){

      final Source src = eachSequence.source();

      final Json source           = Json.object()
        .set("source", String.format("%s#%s", src.className(), src.methodName()));

      final Json actualInvariants = Json.array();
      final Json entrySpace       = Json.array();
      final Json exitSpace        = Json.array();

      final boolean isEntry       = eachSequence.source().isEntry();

      for(KnownInvariant each : eachSequence.content()){

        actualInvariants.add(each.invariantObject().format());
        if(isEntry){
          entrySpace.add(each.typeOf());
        } else {
          exitSpace.add(each.typeOf());
        }

      }


      source.set("invariants",  actualInvariants);
      source.set("entryspace",  entrySpace);
      source.set("exitspace",   exitSpace);

      sources.add(source);

    }

    file.set("sources", sources);

    final Path here = Paths.get("data.json");
    if(Files.exists(here)){
      Files.delete(here);
    }

    final byte[] content = file.toString().getBytes();

    Files.write(here, content);

    //sequences.stream().map(InvariantSequence::normalized).forEach(System.out::println);
  }

  /**
   * Returns a list of non empty segments.
   *
   * @param fromDirectory the source of all these segments.
   * @return a segment list.
   */
  private List<InvariantSequence> sequenceList(Path fromDirectory){
    if(Objects.isNull(fromDirectory) || !Files.exists(fromDirectory)){
      return Collections.emptyList();
    }

    final List<PptMap>            containers  = Utils.mapList(fromDirectory);
    final List<InvariantSequence> result      = new LinkedList<>();

    containers.forEach(c -> result.addAll(Sequences.filtered(c)));

    return result.stream()
      .filter(i -> !i.isEmpty())
      .collect(Collectors.toList());
  }



}
