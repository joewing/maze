#!/usr/bin/perl -w
# Maze generator in Perl
# Joe Wingbermuehle

# The size of the maze (must be odd).
$width = 39;
$height = 23;

@maze = ();

# Display the maze.
sub display_maze {
  for (my $y = 0; $y < $height; $y++) {
    for (my $x = 0; $x < $width; $x++) {
      if ($maze[$y][$x] == 0) { print "  " } else { print "[]" }
    }
    print "\n";
  }
}

# Initialize the maze.
sub init_maze {
  for (my $y = 0; $y < $height; $y++) {
    for (my $x = 0; $x < $width; $x++) {
      $maze[$y][$x] = 1;
    }
  }
}

# Carve the maze starting at x, y.
sub carve_maze {
  my $x = shift;
  my $y = shift;
  $maze[$y][$x] = 0;
  my $dir = int(rand(4)) % 4;
  my $count = 0;
  while ($count < 4) {
    my $dx = 0, $dy = 0;
    if    ($dir == 0) { $dx =  1 }
    elsif ($dir == 1) { $dy =  1 }
    elsif ($dir == 2) { $dx = -1 }
    else              { $dy = -1 }
    my $x1 = $x + $dx;
    my $y1 = $y + $dy;
    my $x2 = $x1 + $dx;
    my $y2 = $y1 + $dy;
    if ($x2 > 0 && $x2 < $width && $y2 > 0 && $y2 < $height) {
      if ($maze[$y1][$x1] == 1 && $maze[$y2][$x2] == 1) {
        $maze[$y1][$x1] = 0;
        carve_maze($x2, $y2);
      }
    }
    $count += 1;
    $dir = ($dir + 1) % 4;
  }
}

sub generate_maze {
  init_maze();
  carve_maze(1, 1);
  $maze[0][1] = 0;
  $maze[$height - 1][$width - 2] = 0;
}

generate_maze();
display_maze();
