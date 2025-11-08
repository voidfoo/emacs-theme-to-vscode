// Color name analyzer
const fs = require('fs').promises;
const path = require('path');

const EMACS_DEFS_DIR = path.join(__dirname, '../../emacs-definitions');

async function analyzeColors() {
  const colors = new Set();
  const files = await fs.readdir(EMACS_DEFS_DIR);
  
  for (const file of files) {
    if (!file.endsWith('.json')) continue;
    
    const filePath = path.join(EMACS_DEFS_DIR, file);
    const content = await fs.readFile(filePath, 'utf8');
    const theme = JSON.parse(content);
    
    for (const face of Object.values(theme)) {
      if (face.fg && typeof face.fg === 'string' && !face.fg.startsWith('#')) {
        colors.add(face.fg.toLowerCase());
      }
      if (face.bg && typeof face.bg === 'string' && !face.bg.startsWith('#')) {
        colors.add(face.bg.toLowerCase());
      }
    }
  }
  
  const sortedColors = [...colors].sort();
  console.log('Found named colors:');
  console.log(JSON.stringify(sortedColors, null, 2));
}

analyzeColors().catch(console.error);