/*
 * Project: days
 * Created Date: Sunday July 9th 2023 4:55:01 pm
 * Author: Fa C Shus (paul@facshus.com)
 * -----
 * Last Modified: Sunday, 9th July 2023 4:55:01 pm
 * Modified By: Fa C Shus (paul@facshus.com)
 * -----
 * Copyright (c) 2021 - 2023 FaCShus Systems
 * License: MIT
 */


// Open filename and return the contents as a list of strings
import 'dart:io';

Future<List<String>> getData(String filename) async {
  final file = File(filename);
  final contents = await file.readAsLines();
  return contents;
}
