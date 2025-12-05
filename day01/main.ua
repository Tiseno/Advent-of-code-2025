&fras "input.1.txt"
▽⊸≠@R            # Remove R
⍜⊜∘ ≡⋅"-" ⊸⦷ "L" # Replace L with -
⊜□ ⊸≠@\n         # Split on newlines
⋕                # Parse into numbers
⊂ 50             # Add initial orientation
\(◿100+)         # Calculate all resulting states
/+ =0            # Count all zeroes
