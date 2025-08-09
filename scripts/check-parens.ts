#!/usr/bin/env -S deno run --allow-read
// scripts/check-parens.ts - Deno script to check for balanced parentheses, brackets, and braces.
// Outputs structured JSON for machine consumption.
// Handles strings and comments to avoid false positives.
// Includes suggestions for fixing errors.

interface StackItem {
  char: string;
  line: number;
  col: number;
}

interface ValidationResult {
  status: "balanced" | "unbalanced";
  error?: {
    type: "unmatched-closing" | "mismatched-closing" | "unclosed-opening";
    char: string;
    line: number;
    col: number;
    message: string;
    suggestion: string;
    offendingFile: string;
    expected?: string;
    opening?: {
      char: string;
      line: number;
      col: number;
    };
  };
  filePath: string;
}

/**
 * Checks if a given file has balanced parentheses, brackets, and braces.
 * @param filePath The path to the file to check.
 */
async function checkBalance(filePath: string): Promise<void> {
  try {
    const content = await Deno.readTextFile(filePath);
    const stack: StackItem[] = [];
    const openChars = "([{";
    const closeChars = ")]}";
    const matchingChars: { [key: string]: string } = {
      ")": "(",
      "]": "[",
      "}": "{",
    };
    const reverseMatchingChars: { [key: string]: string } = {
      "(": ")",
      "[": "]",
      "{": "}",
    };

    const lines = content.split("\n");
    let inString = false;
    let inComment = false;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      inComment = false; // Reset for each line as we only handle single-line comments for now

      for (let j = 0; j < line.length; j++) {
        const char = line[j];
        const prevChar = j > 0 ? line[j - 1] : null;

        // Check for start of a single-line comment
        if (char === ";") {
          inComment = true;
        }

        if (inComment) {
          continue;
        }

        // Check for string literals
        if (char === '"' && prevChar !== "\\") {
          inString = !inString;
          continue;
        }

        if (inString) {
          continue;
        }

        if (openChars.includes(char)) {
          stack.push({ char, line: i + 1, col: j + 1 });
        } else if (closeChars.includes(char)) {
          if (stack.length === 0) {
            const result: ValidationResult = {
              status: "unbalanced",
              error: {
                type: "unmatched-closing",
                char,
                line: i + 1,
                col: j + 1,
                message: `Unmatched closing character '${char}'.`,
                suggestion: `Remove the extra closing character '${char}' or add a corresponding opening character.`,
                offendingFile: filePath,
              },
              filePath,
            };
            console.log(JSON.stringify(result, null, 2));
            return;
          }
          const lastOpen = stack.pop()!;
          if (lastOpen.char !== matchingChars[char]) {
            const result: ValidationResult = {
              status: "unbalanced",
              error: {
                type: "mismatched-closing",
                char,
                line: i + 1,
                col: j + 1,
                message: `Mismatched closing character '${char}'. Expected to close '${lastOpen.char}'.`,
                suggestion: `Replace '${char}' with '${
                  reverseMatchingChars[lastOpen.char]
                }' or check the opening character at line ${
                  lastOpen.line
                }, column ${lastOpen.col}.`,
                offendingFile: filePath,
                expected: lastOpen.char,
                opening: lastOpen,
              },
              filePath,
            };
            console.log(JSON.stringify(result, null, 2));
            return;
          }
        }
      }
    }

    if (stack.length > 0) {
      const lastUnclosed = stack[stack.length - 1];
      const result: ValidationResult = {
        status: "unbalanced",
        error: {
          type: "unclosed-opening",
          char: lastUnclosed.char,
          line: lastUnclosed.line,
          col: lastUnclosed.col,
          message: `Unclosed opening character '${lastUnclosed.char}'.`,
          suggestion: `Add a closing character '${
            reverseMatchingChars[lastUnclosed.char]
          }' for the opening character at line ${lastUnclosed.line}, column ${
            lastUnclosed.col
          }.`,
          offendingFile: filePath,
        },
        filePath,
      };
      console.log(JSON.stringify(result, null, 2));
    } else {
      const result: ValidationResult = {
        status: "balanced",
        filePath,
      };
      console.log(JSON.stringify(result, null, 2));
    }
  } catch (error) {
    if (error instanceof Deno.errors.NotFound) {
      console.error(`Error: File not found at '${filePath}'`);
    } else {
      console.error(`An unexpected error occurred: ${error.message}`);
    }
    Deno.exit(1);
  }
}

// --- Main execution ---
if (Deno.args.length !== 1) {
  console.error("Usage: ./scripts/check-parens.ts <file-to-check>");
  Deno.exit(1);
}

const fileToCheck = Deno.args[0];
checkBalance(fileToCheck);
