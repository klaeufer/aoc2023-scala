import java.nio.file.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;
import java.util.stream.*;

public class Day5j {

  private static final Pattern number = Pattern.compile("(\\d+)");

  public static List<Long> makeSeq(final Iterator<String> input) {
    final var line = input.next();
    final var matcher = number.matcher(line);
    final var result = matcher
        .results()
        .map(MatchResult::group)
        .map(Long::parseLong)
        .collect(Collectors.toList());
    input.next(); // skip blank line
    return result;
  }

  public static Optional<Function<Long, Long>> makeMap(final Iterator<String> input) {
    if (!input.hasNext()) return Optional.empty();

    input.next(); // skip section header

    final var stream = StreamSupport.stream(
        Spliterators.spliteratorUnknownSize(input, Spliterator.ORDERED),
        false);

    final var ranges = stream
        .takeWhile(line -> !line.isEmpty())
        .map(line -> {
          final var matcher = number.matcher(line);
          final var nums = matcher
              .results()
              .map(MatchResult::group)
              .map(Long::parseLong)
              .collect(Collectors.toList());
          return new Triple(nums.get(0), nums.get(1), nums.get(2));
        })
        .collect(Collectors.toList());

    return Optional.of(i ->
        ranges
            .stream()
            .filter(r -> r.start <= i && i < r.start + r.length)
            .findFirst()
            .map(r -> r.base + i - r.start)
            .orElseGet(() -> i)
    );
  }

  static class Triple {
    long base, start, length;

    public Triple(long base, long start, long length) {
      this.base = base;
      this.start = start;
      this.length = length;
    }

    public String toString() {
      final var sb = new StringBuilder();
      sb.append("<b=");
      sb.append(base);
      sb.append(",s=");
      sb.append(start);
      sb.append(",l=");
      sb.append(length);
      sb.append(">");
      return sb.toString();
    }
  }

  // Methods to process parts 1 and 2, and the main method, would follow a similar translation pattern
  // This includes converting iterator operations, mapping, and reducing functions into Java's stream API equivalents where applicable

  public static void main(String[] args) throws Throwable {
    final var input = Files
        .newBufferedReader(Paths.get("data/day5input.txt"))
        .lines()
        .iterator();
    final var seeds = makeSeq(input);
    final var allMaps = Stream
        .generate(() -> makeMap(input))
        .takeWhile(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toList());
    Collections.reverse(allMaps);
    final var seedToLocation = allMaps
        .stream()
        .reduce((f, g) -> s -> f.apply(g.apply(s)))
        .get();
    final var part1 = seeds
        .stream()
        .map(seedToLocation)
        .min(Long::compare)
        .get();
    System.out.println(part1); // 174137457
    https://cr.openjdk.org/~vklang/gatherers/api/java.base/java/util/stream/Gatherers.html
    final var part2 = seeds
        .stream()
        .gather(Gatherers.windowFixed(2))
        .map(p)
        .map(seedToLocation)
        .min(Long::compare)
        .get();


// 1493866
  }
}
