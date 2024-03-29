package main

import (
	"os"
	"strings"
)

type IniSection map[string]string
type IniFile map[string]IniSection

func LoadIniFromString(content string) IniFile {
	ini := make(IniFile)
	var section IniSection

	// Read the file
	for _, line := range strings.Split(content, "\n") {
		// Parse the line
		line = strings.Trim(line, "\r\n\t ")
		if len(line) == 0 {
			continue
		} else if line[0] == ';' || line[0] == '#' {
			continue
		} else if line[0] == '[' {
			line = strings.Trim(line, "[]")
			sectname := strings.Trim(line, "\t ")
			section = make(IniSection)
			ini[sectname] = section
		} else {
			if section == nil {
				section = make(IniSection)
				ini["default"] = section
			}
			pos := strings.Index(line, "=")
			if pos >= 0 {
				key := strings.Trim(line[:pos], "\t ")
				val := strings.Trim(line[pos+1:], "\t ")
				section[key] = val
			}
		}
	}
	// Parse the file
	// Return the data
	return ini
}

// dump ini to string
func DumpIniToString(ini IniFile) string {
	content := make([]string, 0)
	line := ""
	for sectname, section := range ini {
		line = "[" + sectname + "]"
		content = append(content, line)
		for key, val := range section {
			line = key + "=" + val
			content = append(content, line)
		}
		content = append(content, "")
	}
	return strings.Join(content, "\n")
}

func ReadIniFile(filename string) (IniFile, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	return LoadIniFromString(string(content)), nil
}

func WriteIniFile(filename string, ini IniFile) error {
	content := DumpIniToString(ini)
	return os.WriteFile(filename, []byte(content), 0644)
}

func main() {
	ini, err := ReadIniFile("/home/skywind/.vim/vim/tasks.ini")
	if err != nil {
		println("Error reading file:", err.Error())
		return
	}
	println(DumpIniToString(ini))
}
